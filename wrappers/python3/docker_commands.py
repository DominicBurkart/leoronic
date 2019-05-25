import sys

(major, minor, _1, _2, _3) = sys.version_info

command_template = """\
import dill
import base64

result_pipe = open("LEORONIC_RESULT", "w")
f = dill.loads(base64.b64decode("{}".encode()))
try:
    result_pipe.write("r" + base64.b64encode(dill.dumps(f()).encode()).decode())
except Exception as e:
    result_pipe.write("e" + base64.b64encode(dill.dumps(e).encode()).decode())
result_pipe.close()
""".replace(
    "\n", "; "
).replace(
    '"', "'"
)

container_template = f"""\
FROM python:{major}.{minor}
RUN  pip install dill
CMD  python -c "{command_template}"
"""
