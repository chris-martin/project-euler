class Generator
  constructor: (@next) ->

  filter: (predicate) ->
    new Generator =>
      while true
        x = @next()
        return if x == undefined
        return x if predicate(x)

  map: (f) ->
    new Generator =>
      x = @next()
      return f(x) if x != undefined

  flatMap: (f) ->
    gen = undefined
    new Generator =>
      while true
        if gen == undefined
          x = @next()
          return if x == undefined
          gen = f(x)
        else
          x = gen.next()
          if x == undefined
            gen = undefined
          else
            return x

  best: (choose) ->
    m = undefined
    while true
      x = @next()
      return m if x == undefined
      m =
        if m == undefined
          x
        else
          choose(x, m)
    m

  max: ->
    @best(Math.max)

Generator.range = (start, end) ->
  i = start - 1
  new Generator ->
    return if i == end
    i += 1
    i

exports.Generator = Generator;
