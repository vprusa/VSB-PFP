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

    def test_create_for_four_elements(self):
        size = 9
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 2)

    def test_create_for_four_elements(self):
        size = 10
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 3)

    def test_create_for_four_elements(self):
        size = 16
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 3)


    def test_create_for_four_elements(self):
        size = 27
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 3)

    def test_create_for_four_elements(self):
        size = 28
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 3)


    def test_create_for_four_elements(self):
        size = 30
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 3)


    def test_create_for_four_elements(self):
        size = 31
        immutable_array = ImmutableArray(size)
        self.assertEqual(immutable_array.levels, 3)

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