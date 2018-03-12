import socket

leoronic_directory = "~/Documents/cute_coding/leoronic"

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


def __to_task__(task_string, conditions=None, id=None, source=socket.gethostname().split(".")[0],
                directory=leoronic_directory):
    '''

    :param task_string:
    :param conditions:
    :param id:
    :param source: str. should be the unique identifier for this machine.
    :return:
    '''
    import time
    id = str(time.time()) + "_" + str(source)
    raise NotImplementedError
    return id


def __send_tasks__():
    for t in __unsent_tasks__:
        raise NotImplementedError
        # run add_shunt in leoronic_shunts.erl


def wait_for():
    # write an erlang function that terminates when a process has completed.
    raise NotImplementedError


def run_and_return(functions):
    '''
    use functools.partial(func, *args, **keywords) to generate simplified functions

    :param functions: iterable of functions to be called
    :return:
    '''
    import time
    now = str(time.time()).replace(".", "_")
    i = 0
    ids = []
    for f in functions:
        fpickle = o(f, now + "_" + str(i))
        id = __to_task__("python3 funcwrap.py " + fpickle)
        # TODO we need to send_file to this node.
        ids.append(id)
        i += 1
    __send_tasks__()
    map(wait_for, ids)
