import base64
import builtins
import datetime
import functools
import os
import re
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from typing import Set, Iterable, Dict, Callable, TypeVar, List, IO, Iterator

import cloudpickle  # type: ignore
from mypy_extensions import TypedDict

from .worker_commands import make_command

### types

TaskId = str
ClientId = int
Input = TypeVar("Input")
Output = TypeVar("Output")
Fun = Callable[[Input], Output]
Reqs = TypedDict(
    "Reqs",
    {
        "wait": bool,  # if false, result not automatically piped out of leoronic.
        "cpus": int,
        "memory": int,  # in megabytes
        "storage": int,  # in megabytes
        "dockerless": bool,
    },
    total=False,
)


class LeoronicBaseClass:
    pass


class LeoronicError(BaseException, LeoronicBaseClass):
    pass


class UnknownTaskError(LeoronicError):
    pass


class BadInputError(LeoronicError):
    pass


class MessageTypeNotFound(LeoronicError):
    pass


class Settings(LeoronicBaseClass):
    await_unknown_tasks: bool = False
    read_pipe: str = os.path.join(os.path.expanduser("~"), "leoronic_out.pipe")
    write_pipe: str = os.path.join(os.path.expanduser("~"), "leoronic_in.pipe")
    chunk_size: int = 20


@dataclass(frozen=True)
class InputTask(LeoronicBaseClass):
    container: str
    wait: bool = True  # if false, result not automatically piped out of leoronic.
    cpus: int = 1
    memory: int = 500  # in megabytes
    storage: int = 10  # in megabytes
    dockerless: bool = True

    def __str__(self):
        return ", ".join(
            builtins.map(
                lambda v: str(v),
                [
                    id(self),
                    self.wait,
                    self.cpus,
                    self.memory,
                    self.storage,
                    self.dockerless,
                    base64.b64encode(self.container.encode()).decode(),
                ],
            )
        )


@dataclass(frozen=True)
class CompletedTask(LeoronicBaseClass):
    id: str
    created_at: datetime.datetime
    started_at: datetime.datetime
    finished_at: datetime.datetime
    stdout: str
    stderr: str
    result: str


@dataclass(frozen=True)
class AsyncTask(LeoronicBaseClass):
    id: TaskId

    @lru_cache(maxsize=1)
    def get_record(self) -> CompletedTask:
        return get_completed(self.id)

    def get(self) -> Output:
        return handle_completed(self.get_record())


class MessageType(LeoronicBaseClass, Enum):
    new_task_id = "new_task_id"
    task_complete = "task_complete"
    task_not_complete = "task_not_complete"


### fields
settings = Settings()
running_tasks: Dict[TaskId, "InputTask"] = dict()
completed_tasks: Dict[TaskId, "CompletedTask"] = dict()
unclaimed_id_maps: Dict[ClientId, TaskId] = dict()
task_requirements = {}  # type: Reqs


### internal functions


def write_pipe() -> IO[str]:
    return open(settings.write_pipe, mode="w", encoding="ascii")


def read_from_pipe() -> str:
    read = ""
    while len(read) < 2:
        with open(settings.read_pipe, mode="r", encoding="ascii") as pipe:
            read = pipe.readline()
    return read[:-1]


def _parse_str_to_dict(s: str) -> Dict[str, str]:
    return {
        match.group(1): match.group(2)
        for match in re.finditer('([a-zA-Z/_]*)="(.*?)"', s)
    }


def fields(o: object) -> Iterable[str]:
    """only designed to work on classes with type annotations"""
    return o.__annotations__.keys()


def _str_to_date(s: str) -> datetime.datetime:
    return datetime.datetime.utcfromtimestamp(int(s))


def decode(s: str) -> str:
    return base64.b64decode(s.encode()).decode()


def parse_task_response(response: str) -> CompletedTask:
    d = _parse_str_to_dict(response)
    out = decode(d["stdout"])
    err = decode(d["stderr"])
    return CompletedTask(
        id=d["id"],
        created_at=_str_to_date(d["created_at"]),
        started_at=_str_to_date(d["started_at"]),
        finished_at=_str_to_date(d["finished_at"]),
        result=d["result"],
        stdout=out if out == "" else out[:-1],  # removes trailing newline
        stderr=err if err == "" else err[:-1],
    )


def message_type(message: str) -> MessageType:
    for m in MessageType:
        if message.startswith(m.value):
            return m
    raise MessageTypeNotFound


def store_response(message: str) -> None:
    t = message_type(message)

    if t == MessageType.new_task_id:
        _, client_id, task_id = message.split(" ")
        unclaimed_id_maps[int(client_id)] = task_id

    elif t == MessageType.task_complete:
        _, response = message.split(" ", maxsplit=1)
        completed = parse_task_response(response)
        try:
            running_tasks.pop(completed.id)
        except KeyError:
            with open("completed_tasks.txt", "a") as f:
                print(completed_tasks, file=f)
                print("\n\n\n\n\n\n\n", file=f)
            with open("running_tasks.txt", "a") as f:
                print(running_tasks, file=f)
                print("\n\n\n\n\n\n\n", file=f)
            with open("unclaimed_id_maps.txt", "a") as f:
                print(unclaimed_id_maps, f)
                print("\n\n\n\n\n\n\n", file=f)
        completed_tasks[completed.id] = completed

    else:
        raise NotImplementedError


def send_task(task: InputTask) -> TaskId:
    task_id = None

    with write_pipe() as pipe:
        pipe.write(f"add task {str(task)}")

    while task_id is None:
        store_response(read_from_pipe())
        task_id = unclaimed_id_maps.pop(id(task), None)

    running_tasks[task_id] = task
    return task_id


def await_tasks(ids: Set[TaskId]) -> Iterator[CompletedTask]:
    if settings.await_unknown_tasks or any(
        task_id not in running_tasks and task_id not in completed_tasks
        for task_id in ids
    ):
        bad_ids = [
            task_id
            for task_id in ids
            if task_id not in running_tasks and task_id not in completed_tasks
        ]
        raise UnknownTaskError(
            f"Unknown task ids: {bad_ids}. The setting await_unknown_tasks can be "
            f"set to True to allow this API to await unknown leoronic tasks."
        )

    collected = 0
    for task_id in ids.intersection(completed_tasks.keys()):
        yield completed_tasks[task_id]
        collected += 1

    while collected < len(ids):
        store_response(read_from_pipe())
        for task_id in ids.intersection(completed_tasks.keys()):
            yield completed_tasks.pop(task_id)
            collected += 1


def variable_intersection(o1: object, o2: object) -> Set:
    return set(fields(o1)).intersection(set(fields(o2)))


def check_reqs(reqs: Reqs, bad_fields: Iterable[str]) -> None:
    """
    Check that no extra fields are passed for making an InputTask, and that no fields
    that are inappropriate in a given context (passed as "bad_fields") are included.

    :param reqs: a dictionary of field/value pairs to impute into an InputTask.
    :param bad_fields: a list of fields that should not be in reqs.

    :return: None
    :raise: LeoronicError if reqs is misformatted
    """
    bads: List[str] = []

    if any(f in reqs.keys() for f in bad_fields):
        bads.extend(f for f in bad_fields if f in reqs.keys())

    if any(
        req_field not in InputTask.__annotations__.keys() for req_field in reqs.keys()
    ):
        bads.extend(reqs.keys() - InputTask.__annotations__.keys())

    if len(bads) > 0:
        raise BadInputError(f"Bad reqs fields: {bads}")


def handle_completed(task: CompletedTask):
    result = cloudpickle.loads(base64.b64decode(task.result[1:]))
    if task.result[0] == "e":  # error response
        raise result
    return result


def get_completed(task_id: TaskId) -> CompletedTask:
    if task_id in completed_tasks:
        return completed_tasks[task_id]
    return next(await_tasks({task_id}))


def is_pipe_setting(setting_key: str) -> bool:
    return setting_key.endswith("_pipe")


### API


def set_reqs(reqs: Reqs):  # todo untested
    for key in [key for key in reqs]:
        del key  # reqs.clear() is not implemented in TypedDict in 0.4.1
    task_requirements.update(reqs)


def reset_requirements():  # todo untested
    task_requirements.clear()


def apply_async(fun: Fun, *args) -> AsyncTask:
    check_reqs(task_requirements, ["container"])
    task = InputTask(  # type: ignore
        container=make_command(functools.partial(fun, *args)), *task_requirements
    )
    task_id = send_task(task)

    return AsyncTask(id=task_id)


def apply(fun: Fun, *args) -> Output:
    return apply_async(fun, *args).get()  # type: ignore
    # todo: document "container" param issue


def map(fun: Fun, iterable: Iterable[Input]) -> List[Output]:
    return [task.get() for task in map_async(fun, iterable)]


def map_async(fun: Fun, iterable: Iterable) -> List[AsyncTask]:
    return [apply_async(fun, value) for value in iterable]


def imap(
    fun: Fun, iterable: Iterable[Input], chunksize=settings.chunk_size
) -> Iterable[Output]:
    input_exhausted = False
    async_tasks = []
    while not input_exhausted:
        try:
            for _ in range(chunksize):
                async_task = apply_async(fun, next(iter(iterable)))
                async_tasks.append(async_task)
        except StopIteration:
            input_exhausted = True
        for task in async_tasks:
            yield task.get()


def imap_unordered(
    fun: Fun, iterable: Iterable[Input], chunksize=settings.chunk_size
) -> Iterable[Output]:
    input_exhausted = False
    async_tasks = set()
    while not input_exhausted:
        try:
            for _ in range(chunksize):
                async_task = apply_async(fun, next(iter(iterable)))
                async_tasks.add(async_task)
        except StopIteration:
            input_exhausted = True
        for task in async_tasks:
            yield task.get()


def starmap(fun: Fun, iterable: Iterable[Iterable[Input]]) -> Iterable[Output]:
    async_tasks = starmap_async(fun, iterable)
    for task in async_tasks:
        yield task.get()


def starmap_async(fun: Fun, iterable: Iterable[Iterable[Input]]) -> List[AsyncTask]:
    return [apply_async(fun, *value) for value in iterable]
