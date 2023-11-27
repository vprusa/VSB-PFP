class Triple:
    def __init__(self, left, middle, right):
        self.data = [left, middle, right]

class ImmutableArray:
    def __init__(self, size):
        self.size = size
        self.array = self._build_array(size)

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
        self.levels = 0
        self.size = size
        if size == 0:
            # manual definition
            self.levels = 0
            return []
        elif size == 1:
            # manual definition
            self.levels = 1
            return Triple(None, None, None)
        elif size == 2:
            # manual definition
            self.levels = 1
            return Triple(None, None, None)
        elif size == 3:
            # manual definition
            self.levels = 1
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

            # triplets = size // 3
            # self.levels = self._calc_levels(size)
            # split_size = size // 3
            # split_size_rest = size % 3
            # left_size = split_size
            # middle_size = split_size
            # right_size = split_size_rest
            # eq_sizes = size // 3
            # # 12 / 3 = 4
            # # 1 , 1 , 1
            # # middle_size = size / 3
            # # right_size = size / 3
            # # left_size = (size - 1) // 3
            # # right_size = (size - 1) // 3 + (size - 1) % 3
            #
            # left = self._build_array(left_size)
            # if middle_size > 0:
            #     middle = self._build_array(eq_sizes)
            # else:
            #     middle = None  # what if no middle needed
            # if right_size > 0:
            #     right = self._build_array(right_size)
            # else:
            #     right = None  # what if no middle needed
        # else:
        #     left_size = (size - 1) // 3
        #     right_size = (size - 1) // 3 + (size - 1) % 3
        #
        #     left = self._build_array(left_size)
        #     middle = [None]  # Placeholder for a leaf value
        #     right = self._build_array(right_size)

            return [Triple(left, middle, right)]

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