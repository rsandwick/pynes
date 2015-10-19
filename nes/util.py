class enum(tuple):
    __getattr__ = tuple.index

class Record(object):
    def __init__(self, *args):
        for k, v in zip(self.__slots__, args):
            setattr(self, k, v)
