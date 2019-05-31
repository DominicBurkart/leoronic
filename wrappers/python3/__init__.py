import base64
import builtins
import datetime
import functools
import os
import random
import re
import subprocess
from dataclasses import dataclass
from functools import lru_cache
from enum import Enum
from typing import (
    Set,
    Iterable,
    Dict,
    Callable,
    TypeVar,
    List,
    IO,
    Optional,
    Union,
    Iterator,
)

import dill  # type: ignore

from .docker_commands import make_container

### types

TaskId = str
ClientId = int
Input = TypeVar("Input")
Output = TypeVar("Output")
Fun = Callable[[Input], Output]
Reqs = Dict[str, Union[bool, int]]  # InputTask values.


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
    read_pipe: str = os.path.join(os.path.expanduser("~"), "leoronic_in.pipe")
    write_pipe: str = os.path.join(os.path.expanduser("~"), "leoronic_out.pipe")

    def update(self, new: Dict[str, Union[bool, str]]) -> None:
        for name, value in new.items():
            setattr(self, name, value)


@dataclass
class InputTask(LeoronicBaseClass):
    container: str
    _client_id: int = 0  # defined just before the object is piped out
    wait: bool = False
    cpus: int = 1
    memory: int = 500  # in megabytes
    storage: int = 0  # in megabytes
    dockerless: bool = False

    def __str__(self):
        return ", ".join(
            builtins.map(
                lambda v: str(v),
                [
                    self._client_id,
                    self.wait,
                    self.cpus,
                    self.memory,
                    self.storage,
                    self.dockerless,
                    base64.b64encode(self.container.encode()).decode(),
                ],
            )
        )

    def update(self, reqs: Reqs) -> None:
        for name, value in reqs.items():
            setattr(self, name, value)


@dataclass
class CompletedTask(LeoronicBaseClass):
    id: str
    created_at: datetime.datetime
    started_at: datetime.datetime
    finished_at: datetime.datetime
    stdout: str
    stderr: str
    result: str


@dataclass
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
# todo use parallel-friendly dicts https://stackoverflow.com/a/38560235


### internal functions


def write_pipe() -> IO[str]:
    return open(settings.write_pipe, mode="w", encoding="ascii")


def read_pipe() -> IO[str]:
    return open(settings.read_pipe, mode="r", encoding="ascii")


def _parse_str_to_dict(s: str) -> Dict[str, str]:
    return {
        match.group(1): match.group(2)
        for match in re.finditer('([a-zA-Z/_]*)="(.*?)"', s)
    }


def fields(o: object) -> Iterable[str]:
    return o.__annotations__.keys()


def _str_to_date(s: str) -> datetime.datetime:
    """only designed to work on classes with type annotations"""
    return datetime.datetime.utcfromtimestamp(int(s))


def decode(s: str) -> str:
    return base64.b64decode(s.encode()).decode()


def parse_task_response(response: str) -> CompletedTask:
    d = _parse_str_to_dict(response)
    return CompletedTask(
        id=d["id"],
        created_at=_str_to_date(d["created_at"]),
        started_at=_str_to_date(d["started_at"]),
        finished_at=_str_to_date(d["finished_at"]),
        result=decode(d["result"]),
        stdout=decode(d["stdout"]),
        stderr=decode(d["stderr"]),
    )


def get_next_client_id() -> int:
    return (os.getpid() * (10 ** 10)) + random.randint(1, 10 ** 10)


def message_type(message: str) -> MessageType:
    for m in MessageType:
        if message.startswith(m.value):
            return m
    raise MessageTypeNotFound


def store_response(message: str) -> None:
    if message.strip() != "":
        t = message_type(message)

        if t == MessageType.new_task_id:
            _, client, task = message.split(" ")
            unclaimed_id_maps[int(client)] = task

        elif t == MessageType.task_complete:
            _, response = message.split(" ", maxsplit=1)
            completed = parse_task_response(response)
            running_tasks.pop(completed.id)
            completed_tasks[completed.id] = completed

        else:
            raise NotImplementedError


def send_task(task: InputTask) -> TaskId:
    task._client_id = get_next_client_id()
    task_id = None

    with write_pipe() as pipe:
        pipe.write(f"add task {str(task)}")

    with read_pipe() as pipe:
        while task_id is None:
            store_response(pipe.read())
            task_id = unclaimed_id_maps.pop(task._client_id, None)

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

    with read_pipe() as pipe:
        while collected < len(ids):
            store_response(pipe.read())
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
    result = dill.loads(base64.b64decode(task.result[1:].encode()))
    if task.result[0] == "e":  # error response
        raise result
    return result


def get_completed(task_id: TaskId) -> CompletedTask:
    if task_id in completed_tasks:
        return completed_tasks[task_id]
    return next(await_tasks(set(task_id)))


def is_pipe_setting(setting_key: str) -> bool:
    return setting_key.endswith("_pipe")


### API


def apply_async(fun: Fun, *args, reqs: Optional[Reqs] = None, **kwargs) -> AsyncTask:
    if reqs is None:
        reqs = dict()

    check_reqs(reqs, ["container"])
    task = InputTask(container=make_container(functools.partial(fun, *args, **kwargs)))
    task.update(reqs)
    task_id = send_task(task)

    return AsyncTask(id=task_id)


def apply(fun: Fun, *args, reqs: Optional[Reqs] = None, **kwargs) -> Output:
    return apply_async(fun, *args, reqs, **kwargs).get()


def map(
    fun: Fun, iterable: Iterable[Input], reqs: Optional[Reqs] = None
) -> List[Output]:
    return [task.get() for task in map_async(fun, iterable, reqs)]


def map_async(
    fun: Fun, iterable: Iterable, reqs: Optional[Reqs] = None
) -> List[AsyncTask]:
    return [apply_async(fun, value, reqs) for value in iterable]


def imap(
    fun: Fun, iterable: Iterable[Input], reqs: Optional[Reqs] = None
) -> Iterable[Output]:
    for task in map_async(fun, iterable, reqs):
        yield task.get()


def imap_unordered(
    fun: Fun, iterable: Iterable[Input], reqs: Optional[Reqs] = None
) -> Iterable[Output]:
    task_ids = {a.id for a in map_async(fun, iterable, reqs)}
    for completed in await_tasks(task_ids):
        yield handle_completed(completed)


def starmap(
    fun: Fun, iterable: Iterable[Input], reqs: Optional[Reqs] = None
) -> List[Output]:
    return map(fun, iterable, reqs)


def starmap_async(
    fun: Fun, iterable: Iterable[Input], reqs: Optional[Reqs] = None
) -> List[AsyncTask]:
    return map_async(fun, iterable, reqs)
