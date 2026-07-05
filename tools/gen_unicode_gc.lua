-- gen_unicode_gc.lua
--
-- generate unicode_gc_m.f90

local M = {}


function M.open_unicode_gc_m()
   -- open destination file
   local outf = io.open("src/essential/unicode_gc_m.f90", "w")
   if outf == nil then
      io.stderr("Failed to open: src/essential/unicode_gc_m.f90 [gen_unicode_gc.lua]")
   end
   return nil, outf
end

function M.generate_unicode_gc_m(outf, ranges, abbrev_ordered)
   -- Emit the Fortran module.
   outf:write("module forgex_unicode_gc_m\n")
   outf:write("   use :: forgex_segment_m\n")
   outf:write("   implicit none\n")
   outf:write("   private\n\n")

   for _, abbrev in ipairs(abbrev_ordered) do
      outf:write(string.format("   public :: unicode_gc_%s\n", abbrev))
   end
   outf:write("\n")


   for _, abbrev in ipairs(abbrev_ordered) do
      local list = ranges[abbrev]

      outf:write(string.format("   type(segment_t), parameter :: unicode_gc_%s(*) = &\n", abbrev))
     
      outf:write("   [ &\n")
      for i, r in ipairs(list) do
         local comma = (i < #list) and ", &" or " &"
         outf:write(string.format("      segment_t(%d, %d)%s\n", r[1], r[2], comma))
      end
      outf:write("   ]\n\n")
   end

   outf:write("end module forgex_unicode_gc_m\n")
   outf:close()
end

return M
