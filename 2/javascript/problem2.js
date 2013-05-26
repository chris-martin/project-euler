var Generator = require('./Generator.js').Generator;

exports.answer = function () {
  return fibs()
    .takeWhile(function (x) { return x < 4000000; })
    .filter(function (x) { return x % 2 === 0; })
    .sum();
};

function fibs() {
  var x = [ 0, 1 ];
  return new Generator(function() {
    x = [ x[1], x[0]+x[1] ];
    return x[0];
  });
}
