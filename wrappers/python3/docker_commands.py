import sys
import base64
import dill  # type: ignore
from typing import Callable, Any

(major, minor, _1, _2, _3) = sys.version_info

command_template = """\
import dill
import base64
result_pipe = open("LEORONIC_RESULT", "w") # the string LEORONIC_RESULT is replaced in erlang
f = dill.loads(base64.b64decode("{}".encode()))
try:
    result_pipe.write("r" + base64.b64encode(dill.dumps(f()).encode()).decode())
except Exception as e:
    result_pipe.write("e" + base64.b64encode(dill.dumps(e).encode()).decode())
result_pipe.close()
""".replace(
    '"', "'"
)


container_template = f"""\
FROM python:{major}.{minor}
RUN  pip install dill
CMD  python -c "{command_template}"
"""


def make_container(function: Callable[[], Any]) -> str:
    return container_template.format(base64.b64encode(dill.dumps(function)).decode())
