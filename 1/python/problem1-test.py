import problem1
import unittest

class Problem1Test(unittest.TestCase):

  def test_answer(self):
    self.assertEqual(233168, problem1.answer())

if __name__ == '__main__':
    unittest.main()
