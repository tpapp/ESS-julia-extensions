## extensions to ESS, waiting to be merged

module ESSx

VERSION < v"0.4-" && using Docile

export eval_string, ensure_module

@doc doc"""Evaluate `string` as if it was in `filename`, starting at line number `line`. When a `mod` is supplied, evaluation happens in that module.
"""->
function eval_string(string::AbstractString, line::Int, filename::AbstractString,
                     mod::Module=current_module())
  evalstring = "\n"^(line-1)*string
  eval(mod, :(include_string($evalstring, $filename)))
end

@doc """Ensure that a given module, specified as a vector of symbols, exists; by defining it as necessary. Used by ESS when a module containing the evaluated code can be found.
""" ->
function ensure_module(modpath::Array{Symbol,1})
  parent = Main
  for mod in modpath
    try
      if !eval(parent, :(isa($mod, Module)))
        error("$mod already defined as a $(typeof(mod)) in $parent")
      end
    catch
      eval(parent, :(module $(mod) end))
    end
    parent = eval(parent, mod)
  end
  parent
end

@doc """Same as `eval_string` above, but ensures the that the module exists.""" ->
function eval_string(string::AbstractString, line::Int, filename::AbstractString,
                     modpath::Array{Symbol,1})
  eval_string(string, line, filename, ensure_module(modpath))
end

end

## Local Variables:
## eval: (visual-line-mode t)
## End:
