raw_players = nil
File.open('tmp/ratingliste.pre', 'r') do |f|
  raw_players = f.readlines
end

registered_players = raw_players.map do |player|
  player = player.strip
  match = player.match(/\(*#S\(PLAYER :LAST-NAME "(.+?)"/)
  match.nil? ? nil : match[1]
end

puts registered_players.compact!

players = nil
File.open('tmp/ratingliste.pre', 'r') do |f|
  players = f.readlines
end

players = players.map do |player|
  player = player.strip
  match = player.match([0-9]+ \[(.+?)\])
  match.nil? ? nil : match[1]
end

