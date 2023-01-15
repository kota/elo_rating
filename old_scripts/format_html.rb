lines = nil
File.open('players.csv', 'r') do |f|
  lines = f.readlines
end
#lines.shift

players = []
lines.each do |l|
  cols = l.split(',').map(&:strip)
  #players << { number: cols[0], r_number: cols[3], name: cols[2] }
  players << { r_number: cols[0], name: cols[1] }
end

html = nil
File.open('turnering.html', 'r') do |f|
  html = f.read
end

players.sort_by!{|p| p.size }.reverse!
players.each do |p|
  reg = Regexp.new("td\> N#{p[:r_number]}\<")
  html.gsub!(reg, "td> #{p[:name]}<")
  html.gsub!(/Promoting N#{p[:r_number]}  to/, "<tr><td>#{p[:name]}</td><td>")
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
html.gsub!('<td>   1</td><td> 2</td><td> 3</td><td> 4</td>', '<td>1回戦</td><td>2回戦</td><td>4回戦</td>')

header = <<EOS
<html>
  <head>
    <style type="text/css">
      table{
        border-collapse:collapse;
        margin:0 auto;
      }
      th{
        color:#005ab3;
      }
      td{
        border-bottom:1px dashed #999;
      }
      th,tr:last-child td{
        border-bottom:2px solid #005ab3;
      }
      td,th{
        padding:10px;
      }

      .box {
        float: left;
        padding-left: 40px;
      }

      .boxContainer {
        overflow: hidden;
      }

      /* clearfix */
      .boxContainer:before,
      .boxContainer:after {
          content: "";
          display: table;
      }

      .boxContainer:after {
          clear: both;
      }

      /* For IE 6/7 (trigger hasLayout) */
      .boxContainer {
          zoom: 1;
      }
    </style>
    <title>企業将棋部交流リーグ戦</title>
  </head>
  <body>
EOS

footer =  <<EOS
  </body>
</html>
EOS

puts header
puts html
puts footer
