import sys
import base64
import cloudpickle  # type: ignore
from typing import Callable, Any

(major, minor, _1, _2, _3) = sys.version_info

command_template = """\
import pickle
import cloudpickle
import base64
result_pipe = open('LEORONIC_RESULT', 'w')
f = pickle.loads(base64.b64decode('{}'.encode()))
try:
    result_pipe.write('r' + base64.b64encode(cloudpickle.dumps(f())).decode())
except Exception as e:
    result_pipe.write('e' + base64.b64encode(cloudpickle.dumps(e)).decode())
result_pipe.close()
"""

container_template = f"""\
FROM python:{major}.{minor}
RUN pip install cloudpickle
RUN c="{command_template}"
CMD echo -e "$c" | xargs -I % python -c "%"
"""


def make_container(function: Callable[[], Any]) -> str:
    return container_template.format(
        base64.b64encode(cloudpickle.dumps(function)).decode()
    )
