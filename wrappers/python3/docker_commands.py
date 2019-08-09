import sys
import base64
import cloudpickle  # type: ignore
from typing import Callable, Any

(major, minor, _1, _2, _3) = sys.version_info

command_template = """\
import cloudpickle
import base64
f = cloudpickle.loads(base64.b64decode("{}".encode()))
result_pipe = open("/LEORONIC_RESULT", "w")
try:
    result_pipe.write("r" + base64.b64encode(cloudpickle.dumps(f())).decode())
except Exception as e:
    result_pipe.write("e" + base64.b64encode(cloudpickle.dumps(e)).decode())
result_pipe.close()
""".replace(
    "\n", ";"
)

container_template = f"""\
FROM python:{major}.{minor}
RUN pip install cloudpickle
CMD echo '{command_template}' | perl -pe 's/;/\\n/g' | python3 -
"""


def make_container(function: Callable[[], Any]) -> str:
    serialized = base64.b64encode(cloudpickle.dumps(function)).decode()
    return container_template.format(serialized)
