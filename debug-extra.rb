# These routines are required by rdb.el to interface to and enhance the
# standard ruby debugger (debug.rb).

def rdb_bk_valid?(num) 
  # If num is valid, return num-1 ; else nil
  if (num < 1) || (num > DEBUGGER__.break_points.length)
    STDOUT.puts "Breakpoint number #{num} not in range [1..#{DEBUGGER__.break_points.length}]"
    nil
  else
    num-1
  end
end

# Return the breakpoint number at given location or nil.
# If there is none crate one if create argument is true.
# Breakpoint numbers are origin 1.
def rdb_bk_num(file, line, create=true)
  file = File.expand_path(file)
  index = DEBUGGER__.break_points.find_index { |b|
    b[0] and b[1] == 0 and b[2] == file and b[3] == line
  }
  if index
    return index + 1
  elsif create
    b = [true, 0, file, line]
    DEBUGGER__.break_points << b
    index = DEBUGGER__.break_points.length
    return index
  end
  nil
end

def rdb_bk_remove(num)
  return nil unless n = rdb_bk_valid?(num)
  DEBUGGER__.break_points[n][0] = false
  puts "Breakpoint #{num} removed"
end
  
def rdb_bk_list(show_disabled=false)
  n = 0
  active = 0
  DEBUGGER__.break_points.each { |b|
    n += 1              # origin 1
    next unless b[0] || show_disabled    # disabled/removed
    next unless b[1] == 0       # not a breakpoint
    active += 1
    condition = ""
    if b[0].class == String
      condition = "\tif #{b[0]}"
    elsif b[0] == false
      condition = "\tdisabled"
    end
    puts "  #{n}\t#{b[2]}:#{b[3]}#{condition}"
    if b[4]
      puts "  Runs:\t#{b[4]}"
    end
  }
  active
end

def rdb_bk_condition(num)
  return nil unless n = rdb_bk_valid?(num)
  cond = DEBUGGER__.break_points[n][0]
  # if true (no condition) return ""
  cond == true ? "" : cond
end

def rdb_bk_set_condition(num, condition=nil)
  return unless n = rdb_bk_valid?(num)
  if condition
    DEBUGGER__.break_points[n][0] = condition
    puts "Breakpoint #{num} conditional upon #{condition}"
  else
    DEBUGGER__.break_points[n][0] = true
    puts "Breakpoint #{num} now unconditional"
    end
end

def rdb_bk_command(num)
  return nil unless n = rdb_bk_valid?(num)
  command = DEBUGGER__.break_points[n][4]
end

def rdb_bk_set_command(num, command=nil)
  return unless n = rdb_bk_valid?(num)
  if command
    DEBUGGER__.break_points[n][4] = command
    puts "Breakpoint #{num} now runs command #{command}"
  else
    DEBUGGER__.break_points[n][4] = nil
    puts "Breakpoint #{num} command removed"
  end
end

def rdb_zap_breakpoints
  # equiv to DEBUGGER__.break_points = []
  DEBUGGER__.break_points.delete_if { |x| x.length > 0 }
  puts "All breakpoints removed"
end

def rdb_bk_reposition(num, file, line, condition=nil, command=nil)
  while DEBUGGER__.break_points.length < num
     DEBUGGER__.break_points.push [false, 0, "", 0]
    puts "Adding dummy breakpoint #{DEBUGGER__.break_points.length}"
  end
  file = File.expand_path(file)
  if DEBUGGER__.break_points[num-1].nil?
    # this breakpoint did not previously exist
    DEBUGGER__.break_points[num-1] = [condition || true, 0, file, line]
  else
    DEBUGGER__.break_points[num-1][2] = file
    DEBUGGER__.break_points[num-1][3] = line
    DEBUGGER__.break_points[num-1][0] = condition || true
  end
  if command
    if command == "none"
      DEBUGGER__.break_points[num-1].delete_at(4)
    else
      DEBUGGER__.break_points[num-1][4] = command
    end
  end
  puts "breakpoint #{num} now at #{file}:#{line}"
end


# The "require debug" is here, rather than at the top, so if
# rdb is called on an existing inf-ruby buffer then the above commands
# are available at the debugger prompt.

require 'debug.rb'
require 'enhanced-breakpoints.rb'
require 'debug-format-frame.rb'

