--[[
    Lua Filter to parse acronyms in a Markdown document.

    Acronyms must be in the form `\acr{key}` where key is the acronym key.
    The first occurrence of an acronym is replaced by its long name, as
    defined by a list of acronyms in the document's metadata.
    Other occurrences are simply replaced by the acronym's short name.

    A List of Acronym is also generated (similar to a Glossary in LaTeX),
    and all occurrences contain a link to the acronym's definition in this
    List.
]]

-- We want to require the Lua files which are in the same folder.
-- However, as we are invoking this file through Pandoc (and potentially
-- RMarkdown), we do not have control over the `LUA_PATH` environment variable,
-- nor the current working directory.
-- It seems to me that we need to add this current file's directory
-- to the list of searched directories, i.e., `package.path`.
local current_dir = debug.getinfo(1).source:match("@?(.*/)")
package.path = package.path .. ";" .. current_dir .. "/?.lua"

-- The Acronyms database
--[[

    This file defines an Acronym and the Acronyms table.

--]]


-- Define an Acronym with some default values
Acronym = {
    -- The acronym's key, or label. Used to identify it. Must be unique.
    key = nil,
    -- The acronym's short form (i.e., the acronym itself).
    shortname = nil,
    -- The acronym's definition, or description.
    longname = nil,
    -- The number of times this acronym was used.
    occurrences = 0,
    -- The order in which acronyms are defined. 1=first, 2=second, etc.
    definition_order = nil,
    -- The order in which acronyms appear in the document. 1=first, nil=never.
    usage_order = nil,
}


-- Create a new Acronym
function Acronym:new(object)
    setmetatable(object, self)
    self.__index = self

    -- Check that important attributes are non-nil
    assert(object.shortname ~= nil,
        "An Acronym shortname should not be nil!")
    assert(object.longname ~= nil,
        "An Acronym longname should not be nil!")

    -- If the key is not set, we want to use the shortname instead.
    -- (Most of the time, the key is the shortname in lower case anyway...)
    object.key = object.key or object.shortname

    return object
end


-- Debug (helper) function
function Acronym.__tostring(acronym)
    local str = "Acronym{"
    str = str .. "key=" .. acronym.key .. ";"
    str = str .. "short=" .. acronym.shortname .. ";"
    str = str .. "long=" .. acronym.longname .. ";"
    str = str .. "occurrences=" .. acronym.occurrences .. ";"
    str = str .. "definition_order=" .. tostring(acronym.definition_order) .. ";"
    str = str .. "usage_order=" .. tostring(acronym.usage_order)
    str = str .. "}"
    return str
end


-- Increment the count of occurrences
function Acronym:incrementOccurrences()
    self.occurrences = self.occurrences + 1
end


-- Is this the acronym's first occurrence?
function Acronym:isFirstUse()
    return self.occurrences <= 1
end


-- The Acronyms database.
Acronyms = {
    -- The table that contains all acronyms, indexed by their key.
    acronyms = {},

    -- The current "definition_order" value.
    -- Each time a new acronym is defined, we increment this value to keep
    -- count of the order in which acronyms are defined.
    current_definition_order = 0,
}


-- Get the Acronym with the given key, or nil if not found.
function Acronyms:get(key)
    return self.acronyms[key]
end


-- Does the table contains the given key?
function Acronyms:contains(key)
    return self:get(key) ~= nil
end


-- Add a new acronym to the table. Also handles duplicates.
function Acronyms:add(acronym, on_duplicate)
    assert(acronym.key ~= nil,
        "The acronym key should not be nil!")
    assert(on_duplicate ~= nil,
        "on_duplicate should not be nil!")

    -- Handling duplicate keys
    if self:contains(acronym.key) then
        if on_duplicate == "replace" then
            -- Do nothing, let us replace the previous acronym.
        elseif on_duplicate == "keep" then
            -- Do nothing, but do not replace: we return here.
            return
        elseif on_duplicate == "warn" then
            -- Warn, and do not replace.
            warn("Duplicate key: ", acronym.key)
            return
        elseif on_duplicate == "error" then
            -- Stop execution.
            error("Duplicate key: " .. acronym.key)
        else
            error("Unrecognized option on_duplicate = " .. on_duplicate)
        end
    end

    self.current_definition_order = self.current_definition_order + 1
    acronym.definition_order = self.current_definition_order
    self.acronyms[acronym.key] = acronym
end


-- Populate the Acronyms database from a YAML metadata
function Acronyms:parseFromMetadata(metadata, on_duplicate)
    -- We expect the acronyms to be in the `metadata.acronyms.keys` field.
    if not (metadata and metadata.acronyms and metadata.acronyms.keys) then
        return
    end
    -- This field should be a Pandoc "MetaList" (so we can iter over it).
    if not isMetaList(metadata.acronyms.keys) then
        error("The acronyms.keys should be a list!")
    end

    -- Iterate over the defined acronyms. We use `ipairs` since we want to
    -- keep their original order (useful for the `definition_order`!).
    for _, v in ipairs(metadata.acronyms.keys) do
        -- Remember that each of these values can be nil!
        -- By using `and`, we make sure that `stringify` is applied on non-nil.
        local key = v.key and pandoc.utils.stringify(v.key)
        local shortname = v.shortname and pandoc.utils.stringify(v.shortname)
        local longname = v.longname and pandoc.utils.stringify(v.longname)
        local acronym = Acronym:new{
            key = key,
            shortname = shortname,
            longname = longname,
        }
        Acronyms:add(acronym, on_duplicate)
    end
end


-- Populate the Acronyms database from a YAML file
-- Inspired from https://github.com/dsanson/pandoc-abbreviations.lua/
function Acronyms:parseFromYamlFile(filepath, on_duplicate)
    assert(filepath ~= nil,
        "filepath must not be nil!")

    -- First, read the file's content.
    local file = io.open(filepath, "r")
    if file == nil then
        warn("File ", filepath, " could not be read! (does not exist?)")
        return
    end
    local content = file:read("*a")
    file:close()

    -- Secondly, use Pandoc's read ability to parse the content.
    -- Pandoc does not know how to read YAML, so we'll trick it by
    -- asking to parse Markdown instead (since the Markdown's metadata
    -- is YAML anyway).
    local metadata = pandoc.read(content, "markdown").meta

    -- Finally, read the metadata as usual.
    self:parseFromMetadata(metadata, on_duplicate)
end

-- Sorting function
local sortAcronyms = require("sort_acronyms")

-- Replacement function (handling styles)
local replaceExistingAcronymWithStyle = require("acronyms_styles")

-- The options for the List Of Acronyms, as defined in the document's metadata.
local options = {}

--[[
The current "usage order" value.
We increment this value each time we find a new acronym, and we use it
to register the order in which acronyms appear.
--]]
local current_order = 0


-- A helper function to print warnings
function warn(...)
    -- Handle variadic args: use `tostring` to avoid errors
    -- (in particular for table or nil values)
    local t = table.pack(...)
    for i=1, t.n do
        t[i] = tostring(t[i])
    end
    local msg = table.concat(t, "")
    io.stderr:write("[WARNING][acronymsdown] ", msg, "\n")
end

-- Helper function to determine pandoc's version.
-- `version` must be a table of numbers, e.g., `{2, 17, 0, 1}`
function isAtLeastVersion(version)
    -- `PANDOC_VERSION` exists since 2.1, but we never know...
    if PANDOC_VERSION == nil then
        return false
    end
    -- Loop up over the components
    -- e.g., `2.17.0.1` => [0]=2, [1]=17, [2]=0, [3]=1
    for k, v in ipairs(version) do
        if PANDOC_VERSION[k] == nil or PANDOC_VERSION[k] < version[k] then
            -- Examples: 2.17 < 2.17.0.1, or 2.16 < 2.17
            return false
        elseif PANDOC_VERSION[k] > version[k] then
            -- Example: 2.17 > 2.16.2 (we do not need to check the next!)
            return true
        end
    end
    -- At this point, all components are equal
    return true
end


-- Helper function to determine whether a metadata field is a list.
function isMetaList(field)
    -- We want to know whether we have multiple values (MetaList).
    -- Pandoc 2.17 introduced a compatibility-breaking change for this:
    --  the `.tag` is no longer present in >= 2.17 ;
    --  the `pandoc.utils.type` function is only available in >= 2.17
    if isAtLeastVersion({2, 17}) then
        -- Use the new `pandoc.utils.type` function
        return pandoc.utils.type(field) == "List"
    else
        -- Use the (old) `.tag` type attribute
        return field.t == "MetaList"
    end
end


-- Helper function to generate the ID (identifier) from an acronym key.
-- The ID can be used for, e.g., links.
function key_to_id(key)
    return options["id_prefix"] .. key
end
-- Similar helper but for the link itself (based on the ID).
function key_to_link(key)
    return "#" .. key_to_id(key)
end


function Meta(m)
    parseOptionsFromMetadata(m)

    -- Parse acronyms directly from the metadata (`acronyms.keys`)
    Acronyms:parseFromMetadata(m, options["on_duplicate"])

    -- Parse acronyms from external files
    if (m and m.acronyms and m.acronyms.fromfile) then
        if isMetaList(m.acronyms.fromfile) then
            -- We have several files to read
            for _, filepath in ipairs(m.acronyms.fromfile) do
                filepath = pandoc.utils.stringify(filepath)
                Acronyms:parseFromYamlFile(filepath, options["on_duplicate"])
            end
        else
            -- We have a single file
            local filepath = pandoc.utils.stringify(m.acronyms.fromfile)
            Acronyms:parseFromYamlFile(filepath, options["on_duplicate"])
        end
    end

    return nil
end


--[[
Parse the options from the Metadata (i.e., the YAML fields).
Absent options are replaced by a default value.
--]]
function parseOptionsFromMetadata(m)
    options = m.acronyms or {}

    if options["id_prefix"] == nil then
        options["id_prefix"] = "acronyms_"
    else
        options["id_prefix"] = pandoc.utils.stringify(options["id_prefix"])
    end

    if options["sorting"] == nil then
        options["sorting"] = "alphabetical"
    else
        options["sorting"] = pandoc.utils.stringify(options["sorting"])
    end

    if options["loa_title"] == nil then
        options["loa_title"] = pandoc.MetaInlines(pandoc.Str("List Of Acronyms"))
    elseif pandoc.utils.stringify(options["loa_title"]) == "" then
        -- It seems that writing `loa_title: ""` in the YAML returns `{}`
        -- (an empty table). `pandoc.utils.stringify({})` returns `""` as well.
        -- This value indicates that the user does not want a Header.
        options["loa_title"] = ""
    end

    if options["include_unused"] == nil then
        options["include_unused"] = true
    end

    if options["insert_loa"] == false then
        -- Do nothing (keep `insert_loa` = false)
    elseif options["insert_loa"] == nil then
        options["insert_loa"] = "beginning"
    else
        options["insert_loa"] = pandoc.utils.stringify(options["insert_loa"])
    end

    if options["non_existing"] == nil then
        options["non_existing"] = "key"
    else
        options["non_existing"] = pandoc.utils.stringify(options["non_existing"])
    end

    if options["on_duplicate"] == nil then
        options["on_duplicate"] = "warn"
    else
        options["on_duplicate"] = pandoc.utils.stringify(options["on_duplicate"])
    end

    if options["style"] == nil then
        options["style"] = "long-short"
    else
        options["style"] = pandoc.utils.stringify(options["style"])
    end

    if options["insert_links"] == nil then
        options["insert_links"] = true
    end

end


--[[
Generate the List Of Acronyms.
Returns 2 values: the Header, and the DefinitionList.
--]]
function generateLoA()
    -- Original idea from https://gist.github.com/RLesur/e81358c11031d06e40b8fef9fdfb2682

    -- We first get the list of sorted acronyms, according to the defined criteria.
    local sorted = sortAcronyms(Acronyms.acronyms,
            options["sorting"],
            options["include_unused"])

    -- Create the table that represents the DefinitionList
    local definition_list = {}
    for _, acronym in ipairs(sorted) do
        -- The definition's name. A Span with an ID so we can create a link.
        local name = pandoc.Span(acronym.shortname,
            pandoc.Attr(key_to_id(acronym.key), {}, {}))
        -- The definition's value.
        local definition = pandoc.Plain(acronym.longname)
        table.insert(definition_list, { name, definition })
    end

    -- Create the Header (only if the title is not empty)
    local header = nil
    if options["loa_title"] ~= "" then
        local loa_classes = {"loa"}
        header = pandoc.Header(1,
            { table.unpack(options["loa_title"]) },
            pandoc.Attr(key_to_id("HEADER_LOA"), loa_classes, {})
        )
    end

    return header, pandoc.DefinitionList(definition_list)
end


--[[
Append the List Of Acronyms to the document (at the beginning).
--]]
function appendLoA(doc)
    local pos
    if not options["insert_loa"] then
        -- If disabled, do nothing
        return nil
    elseif options["insert_loa"] == "beginning" then
        -- Insert at the first block in the document
        pos = 1
    elseif options["insert_loa"] == "end" then
        -- Insert at the last block in the document
        pos = #doc.blocks + 1
    else
        error("Unrecognized option insert_loa="
                .. tostring(options["insert_loa"]))
    end

    local header, definition_list = generateLoA()

    -- Insert the DefinitionList
    table.insert(doc.blocks, pos, definition_list)

    -- Insert the Header
    if header ~= nil then
        table.insert(doc.blocks, pos, header)
    end

    return pandoc.Pandoc(doc.blocks, doc.meta)
end


--[[
Place the List Of Acronyms in the document (in place of a `\printacronyms` block).
Since Header and DefinitionList are Blocks, we need to replace a Block
(Pandoc does not allow to create Blocks from Inlines).
Thus, `\printacronyms` needs to be in its own Block (no other text!).
--]]
function RawBlock(el)
    -- The block's content must be exactly "\printacronyms"
    if not (el and el.text == "\\printacronyms") then
        return nil
    end

    local header, definition_list = generateLoA()

    if header ~= nil then
        return { header, definition_list }
    else
        return definition_list
    end
end

--[[
Replace an acronym `\acr{KEY}`, where KEY is not in the `acronyms` table.
According to the options, we can either:
- warn, and return simply the KEY as text
- warn, and return "??" as text (similar to bibtex's behaviour)
- raise an error
--]]
function replaceNonExistingAcronym(acr_key)
    -- TODO: adding the source line to warnings would be useful.
    --  But maybe not doable in Pandoc?
    if options["non_existing"] == "key" then
        warn("Acronym key ", acr_key, " not recognized")
        return pandoc.Str(acr_key)
    elseif options["non_existing"] == "??" then
        warn("Acronym key ", acr_key, " not recognized")
        return pandoc.Str("??")
    elseif options["non_existing"] == "error" then
        error("Acronym key " .. tostring(acr_key)
                .. " not recognized, stopping!")
    else
        error("Unrecognized option non_existing="
                .. tostring(options["non_existing"]))
    end
end

--[[
Replace an acronym `\acr{KEY}`, where KEY is recognized in the `acronyms` table.
--]]
function replaceExistingAcronym(acr_key)
    local acronym = Acronyms:get(acr_key)
    acronym:incrementOccurrences()
    if acronym:isFirstUse() then
        -- This acronym never appeared! We first set its usage order.
        current_order = current_order + 1
        acronym.usage_order = current_order
    end

    -- Replace the acronym with the desired style
    return replaceExistingAcronymWithStyle(
        acronym,
        options["style"],
        options["insert_links"]
    )
end

--[[
Replace each `\acr{KEY}` with the correct text and link to the list of acronyms.
--]]
function replaceAcronym(el)
    local acr_key = string.match(el.text, "\\acr{(.+)}")
    if acr_key then
        -- This is an acronym, we need to parse it.
        if Acronyms:contains(acr_key) then
            -- The acronym exists (and is recognized)
            return replaceExistingAcronym(acr_key)
        else
            -- The acronym does not exists
            return replaceNonExistingAcronym(acr_key)
        end
    else
        -- This is not an acronym, return nil to leave it unchanged.
        return nil
    end
end

-- Force the execution of the Meta filter before the RawInline
-- (we need to load the acronyms first!)
-- RawBlock and Doc happen after RawInline so that the actual usage order
-- of acronyms is known (and we can sort the List of Acronyms accordingly)
return {
    { Meta = Meta },
    { RawInline = replaceAcronym },
    { RawBlock = RawBlock },
    { Pandoc = appendLoA },
}
