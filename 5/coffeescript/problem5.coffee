Generator = require('./Generator.coffee').Generator

exports.answer = ->
  Generator.from(1)
    .find (n) ->
      Generator.range(3, 20).all (d) ->
        n % d == 0
