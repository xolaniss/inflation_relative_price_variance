-- Colorizes bracketed spans marked with a `color` attribute, e.g.
--   [some text]{color="red"}
-- Renders as \textcolor{red}{...} in LaTeX/PDF output and as a styled
-- <span> in HTML output. Other output formats (e.g. docx) are left
-- unchanged so citations/content inside the span still render correctly.
function Span(el)
  local color = el.attributes['color']
  if not color then
    return el
  end

  if FORMAT:match('latex') then
    table.insert(el.content, 1, pandoc.RawInline('latex', '\\textcolor{' .. color .. '}{'))
    table.insert(el.content, pandoc.RawInline('latex', '}'))
    return el
  elseif FORMAT:match('html') then
    local style = el.attributes['style']
    el.attributes['style'] = (style and (style .. ' ') or '') .. 'color: ' .. color .. ';'
    return el
  else
    return el
  end
end
