solar <-
    c(
        bg = "#FDF6E3",
        bgdark = "#EDE6CA",
        red = "#DC322F",
        lightorange = "#EBD6C3",
        orange = "#C45B2A",
        yellow = "#A57900",
        lightgreen = "#F0F9B0",
        green = "#859900",
        cyan = "#2AA198",
        lightblue = "#D9EEFF",
        blue = "#268BD2",
        darkblue = "#005BA2",
        warmblue = "#40A0B8",
        violet = "#6C71C4",
        magenta = "#C05068",
        base2 = "#EEE8D5",
        base1 = "#93A1A1",
        base00 = "#657B83",
        base01 = "#586E75",
        base015 = "#174652",
        base02 = "#073642",
        base03 = "#002B36"
        )
set_pars <- function()
{
    graphics::par(
        mar = c(5,5,1,1),
        fg = solar["base01"],
        col.axis = solar["base01"],
        col.lab = solar["base01"],
        col.sub = solar["base01"],
        col.main = solar["base01"]
        )
    par(bg = "#FDF6E3", family = "serif")
}
