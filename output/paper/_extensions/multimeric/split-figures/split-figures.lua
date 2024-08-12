function Div(div)
  if div.identifier:sub(0, 3) == "fig" then
    file = assert(io.open(div.identifier .. "." .. FORMAT, "w"))
    file:write(
      pandoc.write(
        pandoc.Pandoc(div), FORMAT, PANDOC_WRITER_OPTIONS
      )
    )
  end
end
