local M = {}

local function trim(s)
   return s:match("^%s*(.-)%s*$")
end


-- Parse any UCD property file that follows the standard format:
--
--    XXXX           ; PropValue # comment
--    XXXX..YYYY     ; PropValue # comment
--
-- Used for: extracted/DerivedGeneralCategory.txt, Script.txt,
--           PropList.txt, DerivedCoreProperties.txt, etc.
-- 
-- callback(lo, hi, prop) is called for each data line, where lo an hi
-- are integer codepoints (lo == hi for single codepoints) and prop is
-- the trimed property value string.
function M.parse_property_file(path, callback)
   for line in io.lines(path) do
      local data = trim(line:match("^([^#]+)") or "")
      if data == "" then goto continue end

      -- Process only lines containing code points.
      local range, prop = data:match("^([0-9A-Fa-f%.]+)%s*;%s*(%S+)")
      if not range then goto continue end

      -- get one or two HEX codepoint strings.
      local lo_str, hi_str = range:match("^([0-9A-Fa-f]+)%.%.([0-9A-Fa-f]+)$")
      
      -- convert to codepoint number; HEX to DEC.
      local lo, hi
      if lo_str then
         lo = tonumber(lo_str, 16)
         hi = tonumber(hi_str, 16)
      else
         lo = tonumber(range, 16)
         hi = lo
      end

      callback(lo, hi, prop)
      ::continue::
   end

end

-- Parse UnicodeData.txt
function M.parse_unicode_data()
-- NOT IMPLEMENTED
end


-- Parse PropertyValueAliases.txt
function M.parse_property_aliases()
-- NOT IMPLEMENTED
end

return M