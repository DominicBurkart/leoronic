import dill  # type: ignore
import base64
import pipes

t = pipes.Template()
t.append("tr a-z A-Z", "--")
result_pipe = t.open("result", "w")
f = dill.loads(base64.b64decode({}))  # type: ignore
try:
    result_pipe.write("r" + str(base64.b64encode(dill.dumps(f()))))
except Exception as e:
    result_pipe.write("e" + str(base64.b64encode(dill.dumps(e))))
result_pipe.close()
