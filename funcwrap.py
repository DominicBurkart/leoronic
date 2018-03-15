# python3

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


def funcwrap(picklefile, outputfile=None, remove_pickle=True):
    '''
    Loads pickled function from picklefile, runs it with no parameters,
    and pickles the output. Returns filename of output pickle.
    '''
    if outputfile is None:
        outputfile = picklefile + "_output"
    o(l(picklefile)(), outputfile)
    if remove_pickle:
        import os
        os.remove(picklefile)
    return outputfile


if __name__ == "__main__":
    import sys

    if len(sys.argv) >= 2:
        funcwrap(sys.argv[1], outputfile=sys.argv[2])
    else:
        funcwrap(sys.argv[1])