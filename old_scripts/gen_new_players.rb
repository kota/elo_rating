raw_players = nil
File.open('new_players.csv', 'r') do |f|
  raw_players = f.readlines
end
ranks = {
  "2340" => ["5","Dan"] ,
  "2160" => ["4","Dan"] ,
  "2000" => ["3","Dan"] ,
  "1860" => ["2","Dan"] ,
  "1740" => ["1","Dan"] ,
  "1620" => ["1","Kyu"] ,
  "1510" => ["2","Kyu"] ,
  "1410" => ["3","Kyu"] ,
  "1320" => ["4","Kyu"] ,
  "1240" => ["5","Kyu"] ,
  "1160" => ["6","Kyu"] ,
  "1080" => ["7","Kyu"] ,
  "1000" => ["8","Kyu"] ,
  "920"  => ["9","Kyu"] ,
  "840"  => ["10", "Kyu"],
  "760"  => ["11", "Kyu"],
  "680"  => ["12", "Kyu"],
  "600"  => ["13", "Kyu"],
  "520"  => ["14", "Kyu"],
  "440"  => ["15", "Kyu"],
  "?"  => ["??", "??"],
}

players = []
raw_players.each do |p|
  tokens = p.split(',').map{ |s| s.strip }
  rank = ranks[tokens[2]]
  puts "#{tokens[2]}"
  puts "rank = #{rank}"
  players << ["N#{tokens[0]}", "#{rank[0]} \"#{rank[1]}\"" ]
end

puts "("
players.each do |p|
  puts %(("#{p[0]}" "" (#S(HOME :list #\E :nationality "JP" :residens "JP")) #{p[1]} 0 0 nil))
end
puts ")"
