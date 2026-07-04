-- gen_unicode_gc.lua

local ucd = require("tools.ucd_parser")

local UCD_DIR = "ucd/17.0.0/"
local GC_FILE = UCD_DIR .. "extracted/DerivedGeneralCategory.txt"

local ranges = {}
local abbrev_order = {}

ucd.parse_property_file(GC_FILE, function(lo, hi, prop)
   if not ranges[prop] then
      ranges[prop] = {}
      abbrev_order[#abbrev_order+1] = prop
   end
   ranges[prop][#ranges[prop]+1] = {lo, hi}
end)

-- open destination file
local outf = io.open("src/essential/unicode_gc_m.f90", "w")
if outf == nil then
   io.stderr("Failed to open: src/essential/unicode_gc_m.f90 [gen_unicode_gc.lua]")
   return
end

-- Emit the Fortran module.
outf:write("module forgex_unicode_gc_m\n")
outf:write("   use :: forgex_segment_m\n")
outf:write("   implicit none\n")
outf:write("   private\n\n")

for _, abbrev in ipairs(abbrev_order) do
   outf:write(string.format("   public :: unicode_gc_%s\n", abbrev))
end
outf:write("\n")

for _, abbrev in ipairs(abbrev_order) do
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
