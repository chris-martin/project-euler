class Generator
  constructor: (@next) ->

  takeWhile: (predicate) ->
    new Generator =>
      x = @next()
      return x if predicate(x)

  filter: (predicate) ->
    new Generator =>
      while true
        x = @next()
        return if x == undefined
        return x if predicate(x)

  sum: ->
    s = 0
    while true
      x = @next()
      return s if x == undefined
      s += x

exports.Generator = Generator;
