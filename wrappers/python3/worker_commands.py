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


def make_command(function: Callable[[], Any]) -> str:
    serialized = base64.b64encode(cloudpickle.dumps(function)).decode()
    return f"echo '{command_template}' | perl -pe 's/;/\\n/g' | python3 -".format(
        serialized
    )
