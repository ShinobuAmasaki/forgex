-- handle_gc_parse.lua

-- PARSE
local ucd = require("tools.ucd_parser")

local UCD_DIR = "ucd/17.0.0/"
local GC_FILE = UCD_DIR .. "extracted/DerivedGeneralCategory.txt"

local ranges = {}
local abbrev_ordered = {}

ucd.parse_property_file(GC_FILE, function(lo, hi, prop)
   if not ranges[prop] then
      ranges[prop] = {}
      abbrev_ordered[#abbrev_ordered+1] = prop
   end
   ranges[prop][#ranges[prop]+1] = {lo, hi}
end)

-- GENERATE: unicode_gc_m.f90
local gc_m = require("tools.gen_unicode_gc")

local _, outf = gc_m.open_unicode_gc_m()
gc_m.generate_unicode_gc_m(outf, ranges, abbrev_ordered)
outf = nil

-- GENERATE: unicode_tools_m.f90
local tools_m = require("tools.gen_prop2seg")
local inf, outf = tools_m.open_unicode_tools_m()
tools_m.generate_select_constract(abbrev_ordered)
local cases = tools_m.cases
local inserts = {
   PROPERTIY_TO_SEGMENT_LIST = cases
}

require("tools.templater")
render_template(inf, outf, inserts)
