
function render_template(inf, outf, inserts)
   for line in inf:lines() do
      local name = line:match("^%s*!@insert:%s+([A-Z_]+)%s*$")

      if name then
         local block = inserts[name]
         assert(block ~= nil, "missing insert block: " .. name)
         outf:write(block)
         outf:write("\n")
      else
         outf:write(line)
         outf:write("\n")
      end
   end
   inf:close()
   outf:close()
end
