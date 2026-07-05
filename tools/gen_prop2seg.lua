-- gen_prop2seg.lua
--
-- generate unicode_tools_m.f90

local M = {}


function M.open_unicode_tools_m()
   -- open template file
   local inf = io.open("template/unicode_tools_m.template.f90", "r")
   if inf == nil then
      io.stderr("Failed to open: template/unicode_tools_m.template.f90 [gen_prop2seg.lua]")
   end
   
   -- open destination file
   local outf = io.open("src/essential/unicode_tools_m.f90", "w")
   if outf == nil then
      io.stderr("Failed to open: src/essential/unicode_tools_m.f90 [gen_prop2seg.lua]")
   end
   return inf, outf
end

function M.generate_select_constract(abbrevs)
   local case_stat = "      select case (property)\n"
   for _, abbrev in ipairs(abbrevs) do
      case_stat = case_stat .. string.format("         case ('%s')\n", abbrev)
      case_stat = case_stat .. string.format("            seglist = unicode_gc_%s\n", abbrev)
   end
   case_stat = case_stat .. "         case default\n"
   case_stat = case_stat .. "            if (allocated(seglist)) deallocate(seglist)\n"
   case_stat = case_stat .. "            ierr = SYNTAX_INVALID_PROPERTY\n"
   case_stat = case_stat .. "            return\n"
   case_stat = case_stat .. "         end select\n"
   M.cases = case_stat
end

function M.edit()
end

return M
