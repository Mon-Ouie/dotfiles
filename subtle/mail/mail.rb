configure :mail do |s|
  s.interval = 60

  s.icon = Subtlext::Icon.new( "mail.xbm" )

  s.dir = File.join(s.config[:dir] || File.join(Dir.home, "mail"), "inbox/new")
  s.new = if col = s.config[:new]
            Subtlext::Color.new(col)
          else
            Subtlext::Subtle.colors[:urgent]
          end
end

on :run do |s|
  count = Dir.entries(s.dir).size - 2

  if count.zero?
    s.data = s.icon
  else
    s.data = s.new + s.icon
  end
end
