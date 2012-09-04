configure :mail do |s|
  s.interval = 60

  s.icon = Subtlext::Icon.new( "mail.xbm" )

  s.dir = File.join(Dir.home, "mail")
  s.new = if col = s.config[:new]
            Subtlext::Color.new(col)
          else
            Subtlext::Subtle.colors[:urgent_fg]
          end
end

on :run do |s|
  new = Dir.glob("#{s.dir}/**/new/*").any?

  if new
    s.data = s.new + s.icon
  else
    s.data = s.icon.to_s
  end
end
