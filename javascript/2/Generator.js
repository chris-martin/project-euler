function Generator(next) {
  this.next = next;
}

Generator.prototype.takeWhile = function (predicate) {
  var self = this;
  return new Generator(function() {
    var x = self.next();
    if (predicate(x)) return x;
  });
};

Generator.prototype.filter = function (predicate) {
  var self = this;
  return new Generator(function() {
    var x;
    while (true) {
      x = self.next();
      if (x === undefined) return;
      if (predicate(x)) return x;
    }
  });
};

Generator.prototype.sum = function () {
  var self = this, s = 0, x;
  while (true) {
    x = self.next()
    if (x === undefined) return s;
    s += x;
  }
};

exports.Generator = Generator;
