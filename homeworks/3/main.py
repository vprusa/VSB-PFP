class Triple:
    def __init__(self, left, middle, right):
        self.data = [left, middle, right]

class ImmutableArray:
    def __init__(self, size):
        self.size = size
        self.array = self._build_array(size)
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

    def _print(self):
        for t in self.array:
            print(" ", t)
        # for l in range(0, self.levels):
        #     for i in range(0, 3):
        #         print(" ")
        # print ()

    # Prepare structure
    # TODO also fill with data
    def _build_array(self, size):
        # self.size = size
        if size == 0:
            return []
        elif 1 <= size <= 3:
            return Triple(None, None, None)
        else:
            split_size = size // 3
            split_size_rest = size % 3
            left = self._build_array(split_size + split_size_rest)
            if size >= 6:
                middle = self._build_array(split_size)
            else:
                middle = None
            if size >= 9:
                right = self._build_array(split_size)
            else:
                right = None
            return [Triple(left, middle, right)]

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

    def __iter__(self):
        return self._iter(self.array)

    def _iter(self, node):
        if not node:
            return
        for child in node:
            if isinstance(child, Triple):
                yield from self._iter(child.data)
            else:
                yield child

#
# # Example usage:
# size = 10
# immutable_array = ImmutableArray(size)
#
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