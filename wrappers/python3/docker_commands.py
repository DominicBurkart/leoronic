import sys
import base64
import cloudpickle  # type: ignore
from typing import Callable, Any

(major, minor, _1, _2, _3) = sys.version_info

command_template = f"""\
import pickle
import cloudpickle
import base64
result_pipe = open('LEORONIC_RESULT', 'w')
f = pickle.loads(base64.b64decode('{{serialized}}'.encode()))
try:
    result_pipe.write('r' + base64.b64encode(cloudpickle.dumps(f())).decode())
except Exception as e:
    result_pipe.write('e' + base64.b64encode(cloudpickle.dumps(e)).decode())
result_pipe.close()
""".replace(
    "\n", ";"
)

one_line_to_many = """ "${command_template//;/\\n}" """

container_template = f"""\
FROM python:{major}.{minor}
RUN pip install cloudpickle
CMD command_template="{command_template}"
CMD python -c {{one_line_to_many}}
"""


def make_container(function: Callable[[], Any]) -> str:
    serialized = base64.b64encode(cloudpickle.dumps(function)).decode()
    with open("container template.txt", "w") as f:
        f.write(
            container_template.format(
                serialized=serialized, one_line_to_many=one_line_to_many
            )
        )
    return container_template.format(
        serialized=serialized, one_line_to_many=one_line_to_many
    )
