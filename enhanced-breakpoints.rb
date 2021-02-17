# This is a change to part of the standard ruby debugger (debug.rb).

# It assumes that debug-extra has been loaded.
# It implements breakpoint conditions and breakpoint commands.
# Both conditions and comands are strings of ruby that are
# evaluated in the context of the current frame that the
# debugger is sitting at.
#
# A condition of "true" is the same as not having a condition;
# a condition of "nil" or "false" can be used to disable a breakpoint.
# Note that debug.rb uses "false" to remove a breakpoint, so it will
# not appear on debug's `b' command.
#
# The command cannot be a debugger command, just code that can be
# run in the context of the program being debugged.
#
# Breakpoint commands and conditions do not appear when debug lists them,
# use rdb_bk_list to show them.

class DEBUGGER__
  class Context # :nodoc:
    def check_break_points(file, klass, pos, binding, id)
      return false if break_points.empty?
      n = 0
      for b in break_points
        n += 1
        if b[0]           # valid - or conditional
          if b[0].class == String
            result = debug_silent_eval(b[0], binding)
            if ! result
              next
            end
          end
          if b[1] == 0    # breakpoint
            if (b[2] == file and b[3] == pos) or
              (klass and b[2] == klass and b[3] == pos)
              stdout.printf "Breakpoint %d, %s at %s:%s\n", n, debug_funcname(id), file, pos
              return b[4] || true
            end
          elsif b[1] == 1 # watchpoint
            if debug_silent_eval(b[2], binding)
              stdout.printf "Watchpoint %d, %s at %s:%s\n", n, debug_funcname(id), file, pos
              return true
            end
          end
        end
      end
      return false
    end

    def trace_func(event, file, line, id, binding, klass)
      Tracer.trace_func(event, file, line, id, binding, klass) if trace?
      context(Thread.current).check_suspend
      @file = file
      @line = line
      case event
      when 'line'
        frame_set_pos(file, line)
        if !@no_step or @frames.size == @no_step
          @stop_next -= 1
          @stop_next = -1 if @stop_next < 0
        elsif @frames.size < @no_step
          @stop_next = 0          # break here before leaving...
        else
          # nothing to do. skipped.
        end
        if @stop_next == 0 or bs = check_break_points(file, nil, line, binding, id)
          @no_step = nil
          suspend_all
	  if bs.class == String
	    debug_silent_eval(bs, binding)
	  end
          debug_command(file, line, id, binding)
        end

      when 'call'
        @frames.unshift [binding, file, line, id]
        if check_break_points(file, klass, id.id2name, binding, id)
          suspend_all
          debug_command(file, line, id, binding)
        end

      when 'c-call'
        frame_set_pos(file, line)

      when 'class'
        @frames.unshift [binding, file, line, id]

      when 'return', 'end'
        if @frames.size == @finish_pos
          @stop_next = 1
          @finish_pos = 0
        end
        @frames.shift

      when 'raise'
        excn_handle(file, line, id, binding)

      end
      @last_file = file
    end

  end
end
