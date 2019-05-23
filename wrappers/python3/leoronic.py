import base64
import datetime
import functools
import os
import pipes
import random
import re
import sys
from dataclasses import dataclass
from enum import Enum
from typing import (
    Set,
    Iterable,
    Dict,
    Callable,
    TypeVar,
    List,
    Any,
    Optional,
    Union,
    Iterator,
)

import dill  # type: ignore

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


@dataclass
class AsyncTask(LeoronicBaseClass):
    id: TaskId

    def get(self) -> Output:
        return handle_completed(get_completed(self.id))


@dataclass
class InputTask(LeoronicBaseClass):
    container: str
    _client_id: int = 0  # is defined just before the object is piped out
    wait: bool = False
    cpus: int = 1
    memory: int = 500  # in megabytes
    storage: int = 0  # in megabytes
    dockerless: bool = False

    def __str__(self):
        return ", ".join(
            [
                self._client_id,
                self.wait,
                self.cpus,
                self.memory,
                self.storage,
                self.dockerless,
                base64.b64encode(self.container),
            ]
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


class MessageType(Enum, LeoronicBaseClass):
    new_task_id = "new_task_id"
    task_complete = "task_complete"
    task_not_complete = "task_not_complete"


### fields

settings = {
    "await_unknown_tasks": False,
    # ^ when false, checks if a task was sent to leoronic & fails
    "pipe_path": os.path.join(os.path.expanduser("~"), "leoronic.pipe"),
}
running_tasks: Dict[TaskId, "InputTask"] = dict()
completed_tasks: Dict[TaskId, "CompletedTask"] = dict()
unclaimed_id_maps: Dict[ClientId, TaskId] = dict()

### dockerfile container template

(major, minor, _1, _2, _3) = sys.version_info
command_template = open("docker_wrapper_template.py").read().replace("\n", "; ")

container_template = f"""
FROM python:{major}.{minor}
RUN  pip install dill
CMD  python -c "{command_template}"
"""


### internal functions


def leoronic_pipe():  # pipe creation & deletion is handled in erlang.
    t = pipes.Template()
    t.append("tr a-z A-Z", "--")
    return t.open(settings["pipe_path"], "w")


def _parse_str_to_dict(s: str) -> Dict[str, str]:
    return {
        match.group(1): match.group(2)
        for match in re.finditer('([a-zA-Z/_]*)="(.*?)"', s)
    }


def _fields(o: object) -> Iterable[str]:
    return o.__annotations__.keys()


def _str_to_date(s: str) -> datetime.datetime:
    return datetime.datetime.utcfromtimestamp(int(s))


def parse_task_response(response: str) -> CompletedTask:
    d = _parse_str_to_dict(response)
    return CompletedTask(
        id=d["id"],
        created_at=_str_to_date(d["created_at"]),
        started_at=_str_to_date(d["started_at"]),
        finished_at=_str_to_date(d["finished_at"]),
        result=str(base64.b64decode(d["result"])),
        stdout=str(base64.b64decode(d["stdout"])),
        stderr=str(base64.b64decode(d["stdout"])),
    )


def _get_next_client_id() -> int:
    return (os.getpid() * (10 ** 10)) + random.randint(1, 10 ** 10)


def message_type(message: str) -> MessageType:
    for m in MessageType:
        if message.startswith(m.value):
            return m
    raise MessageTypeNotFound


def store_response(message: str) -> None:
    # todo verify that this method (storing messages in dictionaries) is parallel-safe
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
    task._client_id = _get_next_client_id()
    task_id = None

    with leoronic_pipe() as pipe:
        pipe.write(f"add task {str(task)}")
        while task_id is None:
            store_response(pipe.read())
            task_id = unclaimed_id_maps.pop(task._client_id, None)

    running_tasks[task_id] = task
    return task_id


def await_tasks(ids: Set[TaskId]) -> Iterator[CompletedTask]:
    if settings["await_unknown_tasks"] or any(
        task_id not in running_tasks and task_id not in completed_tasks
        for task_id in ids
    ):
        bad_ids = [
            task_id
            for task_id in ids
            if task_id not in running_tasks and task_id not in completed_tasks
        ]
        raise UnknownTaskError(
            f"Unknown task ids: {bad_ids}. The setting await_unknown_tasks can be"
            f"set to True to allow this API to await unknown leoronic tasks."
        )

    collected = 0
    for task_id in ids.intersection(completed_tasks.keys()):
        yield completed_tasks[task_id]
        collected += 1
    with leoronic_pipe() as pipe:
        while collected < len(ids):
            store_response(pipe.read())
            for task_id in ids.intersection(completed_tasks.keys()):
                yield completed_tasks[task_id]
                collected += 1


def make_container(function: Callable[[], Any]) -> str:
    return container_template.format(base64.b64encode(dill.dumps(function)))


def _variable_intersection(o1: object, o2: object) -> Set:
    return set(_fields(o1)).intersection(set(_fields(o2)))


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
    result = dill.loads(base64.b64decode(task.result[1:]))
    if task.result[0] == "e":  # error response
        raise result
    return result


def get_completed(task_id: TaskId) -> CompletedTask:
    if task_id in completed_tasks:
        return completed_tasks[task_id]
    return next(await_tasks(set(task_id)))


### API


def apply(fun: Fun, *args, reqs: Optional[Reqs] = None, **kwargs) -> Output:
    if reqs is None:
        reqs = dict()

    check_reqs(reqs, ["container", "wait"])
    task = InputTask(
        container=make_container(functools.partial(fun, *args, **kwargs)), wait=True
    )
    task.update(reqs)
    id = send_task(task)

    return handle_completed(get_completed(id))


def apply_async(fun: Fun, *args, reqs: Optional[Reqs] = None, **kwargs) -> AsyncTask:
    if reqs is None:
        reqs = dict()

    check_reqs(reqs, ["container"])
    task = InputTask(container=make_container(functools.partial(fun, *args, **kwargs)))
    task.update(reqs)
    task_id = send_task(task)

    return AsyncTask(id=task_id)


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
    asyncs = map_async(fun, iterable, reqs)
    for completed in await_tasks(set(a.id for a in asyncs)):
        yield handle_completed(completed)


def starmap(fun: Fun, iterable: Iterable[Input], reqs: Optional[Reqs]) -> List[Output]:
    return map(fun, iterable, reqs)


def starmap_async(
    fun: Fun, iterable: Iterable[Input], reqs: Optional[Reqs] = None
) -> List[AsyncTask]:
    return map_async(fun, iterable, reqs)


### tests & test helpers


def test_fields():
    class DummyClass:
        v1: str
        v2: int

        def m1(self):
            return self.v1 + str(self.v2)

    d = DummyClass("hi", 10)
    assert list(_fields(d)) == ["v1", "v2"]
    assert list(_fields(DummyClass)) == ["v1", "v2"]


def test_input_task_and_completed_task_have_no_overlapping_fields():
    assert len(_variable_intersection(InputTask, CompletedTask)) == 0


def test_apply():
    assert apply(lambda x: x * x, 5) == 25


def test_apply_async():
    async_call = apply_async(lambda x: x * x, 5)
    assert async_call.get() == 25
    assert async_call.get_stderr() == ""
    assert async_call.get_stdout() == ""


def test_apply_async_print():
    async_print = apply_async(lambda x: print(x), "value of x printed to stdout")
    assert async_print.get() == None
    assert async_print.get_stderr() == ""
    assert async_print.get_stdout() == "value of x printed to stdout"


def test_blocking_functions():
    import multiprocessing

    def f(x: int, y: int):
        return x * y

    inputs = [(x, y) for x in range(5) for y in range(5)]
    starmap_output = starmap(f, inputs)
    map_output = map(f, inputs)
    imap_output = imap(f, inputs)
    imap_unordered_output = imap_unordered(f, inputs)

    with multiprocessing.Pool() as p:
        multi_output = p.starmap(f, inputs)

    assert starmap_output == multi_output
    assert map_output == multi_output
    assert imap_output == multi_output
    assert all(v in multi_output for v in imap_unordered_output)


def test_async_functions():
    import multiprocessing

    def f(x: int, y: int):
        return x * y

    inputs = [(x, y) for x in range(5) for y in range(5)]
    starmap_output = [a.get() for a in starmap_async(f, inputs)]
    map_output = [a.get() for a in map_async(f, inputs)]
    with multiprocessing.Pool() as p:
        multi_output = p.starmap(f, inputs)

    assert starmap_output == multi_output
    assert map_output == multi_output


def test_docker_wrapper_template_has_one_set_of_brackets():
    assert open("docker_wrapper_template.py").read().count("{}") == 1


def _test_parse_task_response():
    test = """\
    id="1"
    created_at="1558540420"
    started_at="1558540425"
    finished_at="1558540430"
    stdout="stdoutbase64string"
    stderr="stderrbase64string"
    result="resultbase64string"
    """.replace(
        "\n", ""
    ).replace(
        " ", ""
    )
    resp = parse_task_response(test)
    assert resp == CompletedTask(
        id=1,
        created_at=datetime.datetime(2019, 5, 22, 15, 53, 40),  # utc
        started_at=datetime.datetime(2019, 5, 22, 15, 53, 45),
        finished_at=datetime.datetime(2019, 5, 22, 15, 53, 50),
        stdout=base64.b64decode("stdoutbase64string"),
        stderr=base64.b64decode("stderrbase64string"),
        result=base64.b64decode("resultbase64string"),
    )
