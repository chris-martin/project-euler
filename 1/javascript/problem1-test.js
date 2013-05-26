var problem1 = require('./problem1.js');

exports.testAnswer = function (test) {
  test.equal(problem1.answer(), 233168);
  test.done();
};

