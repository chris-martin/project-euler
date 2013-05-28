class Generator
  constructor: (@next) ->

  filter: (predicate) ->
    new Generator =>
      while true
        x = @next()
        return if x == undefined
        return x if predicate(x)

  find: (predicate) ->
    @filter(predicate).next()

  all: (predicate) ->
    while true
      x = @next()
      return true if x == undefined
      return false if !predicate(x)

Generator.from = (start) ->
  i = start - 1
  new Generator ->
    i += 1
    i

Generator.range = (start, end) ->
  i = start - 1
  new Generator ->
    return if i == end
    i += 1
    i

exports.Generator = Generator;
