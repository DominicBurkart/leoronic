import os
import pipes
import datetime
from dataclasses import dataclass
from typing import Set, Iterable, Dict

pipe_path = os.path.join(os.path.expanduser("~"), "leoronic.pipe")

TaskId = str

sent_tasks: Dict[TaskId, "InputTask"]
completed_tasks: Set["Task"]


@dataclass
class Task:
    id: str
    created_at: datetime.datetime
    started_at: datetime.datetime
    finished_at: datetime.datetime
    stdout: str
    stderr: str
    result: str
    await: bool
    cpus: int
    memory: int
    storage: int
    dockerless: bool
    container: str


@dataclass
class InputTask:
    container: str
    await: bool = False
    cpus: int = 1
    memory: int = 500  # in megabytes
    storage: int = 500  # in megabytes
    dockerless: bool = False

    def __add__(self, other: CompletedTask) -> Task:
        return Task(**{**vars(other), **vars(self)})


@dataclass
class CompletedTask:
    id: str
    created_at: datetime.datetime
    started_at: datetime.datetime
    finished_at: datetime.datetime
    stdout: str
    stderr: str
    result: str

    def __add__(self, other: InputTask) -> Task:
        return Task(**{**vars(self), **vars(other)})


def leoronic_pipe(pipe_path: str = pipe_path):
    t = pipes.Template()
    t.append("tr a-z A-Z", "--")
    return t.open(pipe_path, "w")


def parse_task_response(response: str) -> CompletedTask:
    raise NotImplementedError


def send_task(task: InputTask) -> TaskId:
    with leoronic_pipe() as pipe:
        pass  # pipe.write('hello world')
    raise NotImplementedError


def await_tasks(ids: Set[TaskId]) -> Iterable[CompletedTask]:
    with leoronic_pipe() as pipe:
        pass
    raise NotImplementedError
