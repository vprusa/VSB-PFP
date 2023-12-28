import unittest

from main import Triple
from main import ImmutableArray

class TestImmutableArray(unittest.TestCase):
    def test_create_for_one_element(self):
        size = 1
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 1)

    def test_create_for_two_elements(self):
        size = 2
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 1)

    def test_create_for_three_elements(self):
        size = 3
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 1)

    def test_create_for_four_elements(self):
        size = 4
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 2)

    def test_create_for_9_elements(self):
        size = 9
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 2)

    def test_create_for_10_elements(self):
        stubs = list(range(1, 11))
        size = len(stubs)
        immutable_array = ImmutableArray(size, stubs)
        self.assertEqual(immutable_array.levels, 3)

    def test_create_for_16_elements(self):
        size = 16
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 3)


    def test_create_for_27_elements(self):
        size = 27
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 3)

    def test_create_for_28_elements(self):
        size = 28
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 4)


    def test_create_for_30_elements(self):
        size = 30
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 4)


    def test_create_for_31_elements(self):
        size = 31
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 4)

    def test_iter_for_10_elements(self):
        stubs = list(range(1, 10))
        size = len(stubs)
        immutable_array = ImmutableArray(size, data=stubs)
        self.assertEqual(immutable_array.levels, 2)
        # iter = immutable_array.enumerate2()
        arr = immutable_array._enumerate(immutable_array._data[0])
        # self.assertEqual(arr[0], 1)
        # self.assertEqual(arr[1], 2)
        iter = immutable_array.enumerate()
        self.assertEqual(next(iter), 1)
        self.assertEqual(next(iter), 2)

    def test_get_nth_element(self):
        data = list(range(1, 10))
        size = len(data)
        immutable_array = ImmutableArray(size, data=data)
        self.assertEqual(immutable_array.levels, 2)
        self.assertEqual(immutable_array.get_value(0), 1)
        self.assertEqual(immutable_array.get_value(1), 2)
        self.assertEqual(immutable_array.get_value(9), 9)

    def test_set_nth_element(self):
        data = list(range(1, 10))
        size = len(data)
        immutable_array = ImmutableArray(size, data=data)
        self.assertEqual(immutable_array.levels, 2)
        for i in range(5, 10): #data:
            new_val = i * 100
            idx = i - 1
            new_immutable_array = immutable_array.set_value(idx, new_val)
            self.assertEqual(immutable_array.get_value(idx), i)  # old value is the same
            self.assertEqual(new_immutable_array.get_value(idx), new_val)  # new value is new

    def test_set_nth_element_of_3_level_data_structure(self):
        data = list(range(1, 27))
        size = len(data)
        immutable_array = ImmutableArray(size, data=data)
        self.assertEqual(immutable_array.levels, 3)
        for i in data:
            new_val = i * 100
            idx = i - 1
            new_immutable_array = immutable_array.set_value(idx, new_val)
            self.assertEqual(immutable_array.get_value(idx), i)  # old value is the same
            self.assertEqual(new_immutable_array.get_value(idx), new_val)  # new value is new


    # def test_add_one_element(self):
    #     size = 1
    #     immutable_array = ImmutableArray(size)
    #     new_value = "New Value"
    #     new_array = immutable_array.set_value(0, new_value)
    #     self.assertEqual(list(new_array), [new_value])
    #
    # def test_add_two_elements(self):
    #     size = 2
    #     immutable_array = ImmutableArray(size)
    #     new_value_1 = "New Value 1"
    #     new_array_1 = immutable_array.set_value(0, new_value_1)
    #     new_value_2 = "New Value 2"
    #     new_array_2 = new_array_1.set_value(1, new_value_2)
    #     self.assertEqual(list(new_array_2), [new_value_1, new_value_2])
    #
    # def test_add_three_elements(self):
    #     size = 3
    #     immutable_array = ImmutableArray(size)
    #     new_value_1 = "New Value 1"
    #     new_array_1 = immutable_array.set_value(0, new_value_1)
    #     new_value_2 = "New Value 2"
    #     new_array_2 = new_array_1.set_value(1, new_value_2)
    #     new_value_3 = "New Value 3"
    #     new_array_3 = new_array_2.set_value(2, new_value_3)
    #     self.assertEqual(list(new_array_3), [new_value_1, new_value_2, new_value_3])
    #
    # def test_add_four_elements(self):
    #     size = 4
    #     immutable_array = ImmutableArray(size)
    #     new_value_1 = "New Value 1"
    #     new_array_1 = immutable_array.set_value(0, new_value_1)
    #     new_value_2 = "New Value 2"
    #     new_array_2 = new_array_1.set_value(1, new_value_2)
    #     new_value_3 = "New Value 3"
    #     new_array_3 = new_array_2.set_value(2, new_value_3)
    #     new_value_4 = "New Value 4"
    #     new_array_4 = new_array_3.set_value(3, new_value_4)
    #
    #     expected_array = [
    #         Triple([new_value_1], [None], [new_value_2]),
    #         [new_value_3, new_value_4]
    #     ]
    #
    #     self.assertEqual(list(new_array_4), expected_array)

if __name__ == '__main__':
    unittest.main()