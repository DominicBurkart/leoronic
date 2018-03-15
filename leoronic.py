import socket
import time

local_leoronic_directory = "~/Documents/cute_coding/leoronic"

host = socket.gethostname().split(".")[0]

__unsent_tasks__ = []


def l(file_path):
    import os, pickle
    max_bytes = 2 ** 31 - 1
    bytes_in = bytearray(0)
    input_size = os.path.getsize(file_path)
    with open(file_path, 'rb') as f_in:
        for _ in range(0, input_size, max_bytes):
            bytes_in += f_in.read(max_bytes)
    return pickle.loads(bytes_in)


def o(obj, fname):
    import pickle
    n_bytes = 2 ** 31
    max_bytes = 2 ** 31 - 1
    bytes_out = pickle.dumps(obj)
    with open(fname, 'wb') as f_out:
        for idx in range(0, n_bytes, max_bytes):
            f_out.write(bytes_out[idx:idx + max_bytes])
    return fname


def __to_task__(task_string, conditions=None, necessary_files=None,
                id=None, source=host,
                change_filenames=True, directory=local_leoronic_directory):
    '''

    :param task_string: commands to be run on leoronic.
    :param conditions: conditions necessary for the job to be run.
    :param id: optional custom id for the job.
    :param source: str. should be the unique identifier for this machine.
    :return:
    '''

    # input validation
    try:
        assert task_string is not None and type(task_string) == str
        assert "\n" not in task_string
        assert '"' not in task_string

        assert source is not None and type(source) == str
        assert "\n" not in source
        assert '"' not in source

        if conditions is not None:
            assert "\n" not in conditions
            assert '"' not in conditions

        if necessary_files is not None:
            assert "\n" not in necessary_files
            assert '"' not in necessary_files

    except AssertionError:
        raise TypeError("Bad arguments passed to leoronic.__to_task__. Assertion failed – see output above.")

    id = str(time.time()) + "_" + source

    raise NotImplementedError
    # __unsent_tasks__.append(starg)
    # return id


def __send_tasks__():
    import subprocess
    outstr = "\n".join(__unsent_tasks__)
    cd = 'cd ' + local_leoronic_directory
    imp = 'source leoronic_utilities'
    v = 'leoronic_add_tasks_from_str "' + outstr + '"'
    subprocess.run(";".join([cd, imp, v]), shell=True, check=True)


def wait_for_all(ids, ignore_unknown=False):
    raise NotImplementedError


def collect_leoronic_file(basename, delete=True):  # todo returns absolute path of local file after transferring it.
    raise NotImplementedError


def run_and_return(functions):
    '''
    use functools.partial(func, *args, **keywords) to generate functions with parameters already inside of them.

    :param functions: iterable of functions to be called
    :return:
    '''
    import time
    import os
    now = str(time.time()).replace(".", "_")
    i = 0
    ids = []
    results = []
    for f in functions:
        basename = o(f, now + "_" + host + "_" + str(i))  # writes a pickle file and returns the file's basename
        fpickle = os.path.abspath(basename)  # absolute path to the pickle we just made
        res = basename + "_output"
        id = __to_task__("python3 funcwrap.py " + fpickle + " " + res, necessary_files=[fpickle])
        ids.append(id)
        results.append(res)
        i += 1
    __send_tasks__()
    wait_for_all(ids)
    return [l(collect_leoronic_file(res)) for res in results]  # collects and unpickles each result pickle file.
