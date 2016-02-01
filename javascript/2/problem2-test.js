var problem2 = require('./problem2.js');

exports.testAnswer = function (test) {
  test.equal(problem2.answer(), 4613732);
  test.done();
};

