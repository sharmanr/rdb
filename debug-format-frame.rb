# This is a change to part of the standard ruby debugger (debug.rb).

# It assumes that debug-extra has been loaded.

# If in emacs, output lines showing file and line number when
# line changes, on up and down commands.
class DEBUGGER__
  class Context # :nodoc:
    def format_frame(pos)
      _, file, line, id = @frames[pos]
      if ENV['EMACS']
        stdout.printf "\032\032%s:%d:\n", file, line
      end
      sprintf "#%d %s:%s%s\n", pos + 1, file, line,
        (id ? ":in `#{id.id2name}'" : "")
    end
  end
end
