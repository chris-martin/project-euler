require "./problem1"
require "test/unit"

class TestProblem1 < Test::Unit::TestCase

  def test_simple
    assert_equal(233168, answer())
  end

end