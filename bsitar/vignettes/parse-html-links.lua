local elements_in_link = {}
local link_start
local link_end

Inline = function (el)
  if el.t == 'RawInline' and el.format:match'html.*' then
    if el.text:match'<a ' then
      link_start = el.text
      return {}
    end
    if el.text:match'</a' then
      link_end = el.text
      local link = pandoc.read(link_start .. link_end, 'html').blocks[1].content[1]
      link.content = elements_in_link
      -- reset
      elements_in_link, link_start, link_end = {}, nil, nil
      return link
    end
  end
  -- collect link content
  if link_start then
    table.insert(elements_in_link, el)
    return {}
  end
  -- keep original element
  return nil
end