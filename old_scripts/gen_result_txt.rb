# result.csv.sampleの形式の結果をresult.txtに変換する

lines = nil
File.open('result.csv', 'r') do |f|
  lines = f.readlines
end
lines.shift

$players = []
lines.each do |l|
  cols = l.split(',').map(&:strip)
  $players << { number: cols[0], r_number: cols[3] }
end

def find_number(r_number)
  $players.each do |p|
    return p[:number] if r_number == p[:r_number]
  end
  raise "Number not found for rnumber #{r_number}"
end

def game_result(result_text)
  return '0-' if result_text.nil? || result_text == ''
  return result_text
end

results = []
lines.each do |l|
  cols = l.split(',').map(&:strip)
  r_number = cols[3]
  #results << [number, game_result(cols[4]), game_result(cols[5]), game_result(cols[6])]
  results << [r_number, game_result(cols[4]), game_result(cols[5]), game_result(cols[6]), game_result(cols[7])]
end

results.each do |r|
  puts r.join(',')
end
