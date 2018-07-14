lines = []
while line = STDIN.gets
  lines << line 
end
#puts lines

players = []
lines.each_with_index do |line, i|
  elements = line.split(',')
  players << [elements[0], elements[1..-1]] 
end

results = []
players.each_with_index do |player, i|
  result = []
  player[1].each do |game|
    game.strip!
    if game == '0-'
      result << game  
      next
    end
    opponent = game[0..-2]
    game_result = game[-1]
    opponent_index = players.index(players.find { |pl| pl[0] == opponent })
    #puts "op:#{opponent}"
    result << "#{opponent_index+1}#{game_result}"
    #result << "#{opponent}#{game_result}"
  end
  results << "#{i+1} [N#{player[0]}] [] [#{result.join(' ')}]"
  #results << "#{player[0]} [N#{player[0]}] [] [#{result.join(' ')}]"
end

puts results
File.open("./turnering.txt", "w") do |f| 
  f.puts("[XXth Nekomadoken]")
  f.puts("[2017-00-00]")
  f.puts(results)
end
