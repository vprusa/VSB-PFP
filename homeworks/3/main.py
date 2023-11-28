from array import array


class Triple:
    def __init__(self, left, middle, right):
        self.data = [left, middle, right]

class ImmutableArray:
    def __init__(self, size, stubs=None):
        self.size = size
        self.array = self._build_array(size, stubs)
        self.levels = self._calc_levels(size)

    def _calc_levels(self, size):
        if size <= 3:
            return 1
        res = size
        levels = 0
        while res > 1:
            res = res / 3
            levels = levels + 1
        return levels

    def print(self):
        print("")
        self._print(self.array[0])

    def _print(self, node=None):
        if not node:
            print(f" X", end='')
            return
        if isinstance(node, Triple):
            print(f" ", end='')
            self._print(node.data[0])
            print(f" ", end='')
            self._print(node.data[1])
            print(f" ", end='')
            self._print(node.data[2])
            print(" ")
        else:
            # print(f" [{node[0]}|{node[1]}|{node[2]}]")
            if isinstance(node, int):
                # print(f" {node}", end='')
                print(f"{node}", end='')
            else:
                print(f"T")
        # for t in self.array:
        #     print(" ", t)
        # for l in range(0, self.levels):
        #     for i in range(0, 3):
        #         print(" ")
        # print ()

    # Prepare structure
    # TODO also fill with data
    # Constructor - a way to define an array of given size n (an array to store n values).
    def _build_array(self, size, stubs=None):
        # self.size = size
        if size == 0:
            return []
        elif 1 <= size <= 3:
            if not stubs:
                return Triple(None, None, None)
            elif len(stubs) > 2:
                return Triple(stubs[0], stubs[1], stubs[2])
        else:
            split_size = size // 3
            split_size_rest = size % 3
            # left = self._build_array(split_size + split_size_rest, stubs)
            left_size = split_size + split_size_rest
            if not stubs:
                left = self._build_array(left_size)
            else:
                left = self._build_array(left_size, stubs[0:left_size])
            if size >= 6:
                if not stubs:
                    middle = self._build_array(split_size)
                else:
                    middle = self._build_array(split_size, stubs[left_size:(left_size+split_size)])
            else:
                if not stubs:
                    middle = None
                else:
                    middle = stubs[1]
            if size >= 9:
                # right = self._build_array(split_size)
                if not stubs:
                    right = self._build_array(split_size)
                else:
                    right = self._build_array(split_size, stubs[(left_size+split_size):])

            else:
                if not stubs:
                    right = None
                else:
                    right = stubs[2]
            return [Triple(left, middle, right)]

    # Indexer - a way how to get a value defined by its index (in range 0..n-1).
    def get_value(self, index, node=None):
        if index > self.size:
            raise IndexError(f"Index {index} out of range {self.size} for the array.")
        split_size = self.size // 3
        split_size_rest = self.size % 3
        left_index_max = split_size + split_size_rest
        middle_index_max = split_size * 2
        if isinstance(node, Triple):
            if left_index_max > index:
                return self.get_value(left_index_max - index, node.data[0])
            elif middle_index_max > index:
                return self.get_value(left_index_max - index, node.data[1])
            else:
                return self.get_value(left_index_max-index, node.data[2])
        else:
            return node

    # def __iter__(self):
    #     res = self.enumerate(self.array)
    #     return res

    def enumerate2(self):
        res = self.enumerate(self.array)
        return res

    # Enumerator - a way to go through the array and get all values.
    def enumerate(self, node):
        result = []
        if isinstance(node, Triple):
            res_left = self.enumerate(node.data[0])
            res_middle = self.enumerate(node.data[1])
            res_right = self.enumerate(node.data[2])
            res = list()
            if res_left is not None:
                res = res.extend(res_left)
            if res_middle is not None:
                res.extend(res_middle)
            if res_right is not None:
                res.extend(res_right)
            return res
        else:
            return [node]

    # Set method - a way, ho to change a value in the array based on its index.
    # While it is an immutable array, this method needs to return the new array that accommodated the change.
    def set_value(self, index, value, old_data):
        # copy-past as much from the datastructure as possible
        return
#
# # Example usage:
# size = 10
# stub_data = list(range(1, 20))
# immutable_array = ImmutableArray(size, stub_data)
# print("Print structure\n")
# immutable_array.print()
# #
# # Print values using enumerator
# print("Values in the array:")
# for value in immutable_array:
#     print(value)
#
# # Access values using indexer
# print("\nAccessing values by index:")
# for i in range(size):
#     print(f"Index {i}: {immutable_array[i]}")
#
# # Set a new value at a specific index
# index_to_set = 3
# new_value = "New Value"
# new_array = immutable_array.set_value(index_to_set, new_value)
#
# # Print the updated array
# print(f"\nSetting a new value at index {index_to_set}:")
# for value in new_array:
#     print(value)