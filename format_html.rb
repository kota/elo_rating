lines = nil
File.open('result.csv', 'r') do |f|
  lines = f.readlines
end
lines.shift

players = []
lines.each do |l|
  cols = l.split(',').map(&:strip)
  players << { number: cols[0], r_number: cols[3], name: cols[2] }
end

html = nil
File.open('turnering.html', 'r') do |f|
  html = f.read
end

players.sort_by!{|p| p.size }.reverse!
players.each do |p|
  reg = Regexp.new("td\> N#{p[:r_number]}\<")
  html.gsub!(reg, "td> #{p[:name]}<")
end

grades = [
  ["4 Dan", "四段"],
  ["3 Dan", "三段"],
  ["2 Dan", "二段"],
  ["1 Dan", "初段"],
  ["1 Kyu", "1級"],
  ["2 Kyu", "2級"],
  ["3 Kyu", "3級"],
  ["4 Kyu", "4級"],
  ["5 Kyu", "5級"],
  ["6 Kyu", "6級"],
  ["7 Kyu", "7級"],
  ["8 Kyu", "8級"],
  ["9 Kyu", "9級"],
]

html.gsub!('Nr', 'No.')
html.gsub!('Name', '名前')
html.gsub!('Nat', '国')
html.gsub!('Grade', '段級')
grades.each do |gr|
  html.gsub!(gr[0], gr[1])
end
html.gsub!('ELO', 'ELOレーティング')
html.gsub!('Pts', '勝ち数')
html.gsub!('<td>   1</td><td> 2</td><td> 3</td>', '<td>1回戦</td><td>2回戦</td><td>3回戦</td>')

style = <<EOS
<style type="text/css">

table, td, th { 
border: 1px #2b2b2b solid; 
border-collapse: collapse;
}

</style>
EOS

puts style
puts html
