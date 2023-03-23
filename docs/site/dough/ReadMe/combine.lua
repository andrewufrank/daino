function Reader(inputs, opts)
    -- provided in stackoverflow https://stackoverflow.com/questions/75806890/how-to-combine-some-markdown-with-yaml-header-files-into-a-single-pandoc-outpu/75810635#75810635
    -- by tarleb

    -- use with  pandoc --from=combine.lua index.md 01.md ....
    local doc = pandoc.Pandoc{}  -- the resulting document
    
    -- parse input as Markdown
    local parse = function (input)
        return pandoc.read(tostring(input), 'markdown', opts)
    end
    
    -- The first file is assumed to be special. Just use as-is.
    doc = doc .. parse(inputs:remove(1))
    
    -- Process each input file separately and merge it into the top-level
    -- document.
    for i, input in ipairs(inputs) do
        local part = parse(input, opts)
        -- add the title as a top-level heading
        if part.meta.title then
        doc.blocks:insert(pandoc.Header(1, part.meta.title))
        part.meta.title = nil  -- unset, so it won't conflict with main title
        end
        -- add the abstract under a new heading
        if part.meta.abstract then
        doc.blocks:insert(pandoc.Header(2, 'Abstract'))
        doc.blocks:extend(
            pandoc.utils.type(part.meta.abstract) == 'Inlines' and
            {pandoc.Plain(part.meta.abstract)} or
            part.meta.abstract
        )
        part.meta.abstract = nil  -- prevent conflicts
        end
        -- append the main contents to the result doc and merge all meta
        -- information. Shift headings in the part.
        doc = doc .. part:walk {
        Header = function (h)
            h.level = h.level + 1
            return h
        end
        }
    end
    return doc
    end
      