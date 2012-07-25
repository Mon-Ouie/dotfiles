# -*- encoding: utf-8 -*-
# Maildir specification file
# Created with sur-0.2
Sur::Specification.new do |s|
  # Sublet information
  s.name        = "Mail"
  s.version     = "0.1"

  s.tags        = ["Mail", "Icon"]
  s.files       = ["mail.rb"]
  s.icons       = ["mail.xbm"]

  # Sublet description
  s.description = "Notififes you if you got mail"
  s.notes       = <<NOTES
This sublet gets shiny when you got mail.
NOTES

  # Sublet authors
  s.authors     = ["Mon ouïe"]
  s.contact     = "mon.ouie@mail.com"
  s.date        = " Mon May 02 13:02 MYT 2011"

  # Sublet config
  s.config = [
    {
      :name        => "dir",
      :type        => "string",
      :description => "Path to mailbox",
      :def_value   => "$HOME/mail"
    },
    {
      :name        => "new",
      :type        => "string",
      :description => "Color when you got mail",
      :def_value   => "#ff000"
    }
  ]
end
