from ..wrappers import *
import base64
import datetime


def test_fields():
    class DummyClass:
        v1: str = "hi"
        v2: int = 10

        def m1(self):
            return self.v1 + str(self.v2)

    d = DummyClass()
    assert list(fields(d)) == ["v1", "v2"]
    assert list(fields(DummyClass)) == ["v1", "v2"]


def test_input_task_and_completed_task_have_no_overlapping_fields():
    assert len(variable_intersection(InputTask, CompletedTask)) == 0


def test_docker_wrapper_template_has_one_set_of_brackets():
    assert container_template.count("{}") == 1


def test_parse_task_response():
    stdout = "test stdout"
    stderr = "test stderr"
    result = "test result"

    test = f"""
        id="1"
        created_at="1558540420"
        started_at="1558540425"
        finished_at="1558540430"
        stdout="{base64.b64encode(stdout.encode()).decode()}"
        stderr="{base64.b64encode(stderr.encode()).decode()}"
        result="{base64.b64encode(result.encode()).decode()}"
    """.replace(
        "\n", ""
    ).replace(
        " ", ""
    )

    resp = parse_task_response(test)

    assert resp == CompletedTask(
        id="1",
        created_at=datetime.datetime(2019, 5, 22, 15, 53, 40),  # utc
        started_at=datetime.datetime(2019, 5, 22, 15, 53, 45),
        finished_at=datetime.datetime(2019, 5, 22, 15, 53, 50),
        stdout=stdout,
        stderr=stderr,
        result=result,
    )
