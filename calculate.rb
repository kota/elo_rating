require 'fileutils'

# format_html
# write result csv

class RatingCalculator
  RANKS = {
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

  DANKYU = {
    'Dan' => '段',
    'Kyu' => '級'
  }
 
  def run
    FileUtils.rm_rf('tmp') if File.exist?('./tmp')
    Dir.mkdir('tmp')
    prepare_past_data
    generate_input_files
    `clisp turnering.lisp`
    format_result
    archive_result
  end

  private

  def archive_result
    FileUtils.mv('tmp', "archives/#{@tournament_date}")
  end

  def format_result
    lines = File.readlines('tmp/all.txt')
    lines = lines[3..-1]
    players = []
    lines.each do |line|
      cols = line.split
      name = cols[1]
      if cols.size == 9
        grade1 = cols[2]
        grade2 = DANKYU[cols[3]]
        rating = cols[6]
      elsif cols.size == 7
        grade1 = ''
        grade2 = ''
        rating = cols[4]
      else
        raise '結果のフォーマットが不正です'
      end
      players << [name, rating, grade1 + grade2]
    end
    File.open('tmp/rating_result.csv', 'w') do |f|
      players.sort_by{ |pl| pl[0] }.each do |player|
        f.puts player.join(',')
      end
    end
  end

  def prepare_past_data
    dirs = Dir.glob("archives/*/")
    dirs = dirs.sort!.select { |d| d =~ /[0-9]{8}/ }
    latest = dirs[-1]
    FileUtils.cp(File.join(latest, 'ratingliste.post'), "tmp/ratingliste.pre")
  end

  def list_registered_players
    raw_players = nil
    File.open('tmp/ratingliste.pre', 'r') do |f|
      raw_players = f.readlines
    end
    
    registered_players = raw_players.map do |player|
      player = player.strip
      match = player.match(/\(*#S\(PLAYER :LAST-NAME "(.+?)"/)
      match.nil? ? nil : match[1]
    end
  
    registered_players.compact
  end

  def write_new_players(new_players)
    File.open('tmp/nye-spillere', 'w') do |f|
      f.puts "("
      new_players.each do |player|
        rank = RANKS[player[1]]
        raise "開始Rが不正です #{player[0]} #{player[1]}" unless rank
        rank_str = "#{rank[0]} \"#{rank[1]}\""
        f.puts %(("N#{player[0]}" "" (#S(HOME :list #\\E :nationality "JP" :residens "JP")) #{rank_str} 0 0 nil))
      end
      f.puts ")"
    end
  end

  def generate_input_files
    lines = []
    File.readlines('result.csv').each do |line|
      lines << line.split(',')
    end

    tournament_name = lines[0][2]
    @tournament_date = lines[1][2]
    game_result_start_at = lines.index{ |l| l[0].strip == 'R番号' } + 1
    
    game_result = lines[game_result_start_at..-1]
    
    players = []
    game_result.each_with_index do |line, i|
      players << [line[0], line[3..-1], line[2]]
    end
    
    registered_players = list_registered_players
    
    new_players = []
    results = []
    players.each_with_index do |player, i|
      unless registered_players.include?("N#{player[0]}")
        new_players << [player[0], player[2]] 
      end
    
      result = []
      (0..(player[1].size - 1)).step(3).each do |index|
        game = player[1][index..index+2]
        game = game.map(&:strip)
        if game[0] == '-'
          result << '0-'  
          next
        end
        opponent = game[0]
        game_result = case game[2]
                      when '〇'
                        '+'
                      when '×'
                        '-'
                      when '▲'
                        '='
                      else
                        raise "対戦結果が不正です #{i+1}行目, #{index / 3 + 1}試合"
                      end
        opponent_index = players.index(players.find { |pl| pl[0].to_i == opponent.to_i })
        raise "対戦相手が見つかりません #{i+1}行目, #{index / 3 + 1}試合 相手#{opponent}" if opponent_index == nil
        #puts "op:#{opponent}"
        result << "#{opponent_index+1}#{game_result}"
        #result << "#{opponent}#{game_result}"
        #
      end
      results << "#{i+1} [N#{player[0]}] [] [#{result.join(' ')}]"
      #results << "#{player[0]} [N#{player[0]}] [] [#{result.join(' ')}]"
    end
    
    File.open("./tmp/turnering.txt", "w") do |f| 
      f.puts("[#{tournament_name}]")
      f.puts("[#{@tournament_date[0..3]}-#{@tournament_date[4..5]}-#{@tournament_date[5..6]}]")
      f.puts(results)
    end
    
    write_new_players(new_players)
  end
end

RatingCalculator.new.run
