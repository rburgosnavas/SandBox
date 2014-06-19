import utils.Utils
val indiLogo = Utils.rsz(2730, 687)_


val withRatio = indiLogo(7)


withRatio(_ * _)
withRatio(_ / _)

Utils.rszRng(128, 45)(List.range(1, 10))((x, y) => x * y)

