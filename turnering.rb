$normal_output = true

def run(berger=nil)
  $handicap = nil
  $origin = nil
  $reordering = :undefined
  $inverted_macmahon = false
  $special_promotions = nil
  $delayed_promotions = nil
  $importance = 1

  les("turnering.txt")
  read_table
  calculate
  find_players
  origin = find_origin unless origin

  #reorder(berger)
  #show_players
  plus_minus
  #special_promotions
  #empty_simultan
  #change_us_names if origin =~ /^A/
  #skriv1
  #skriv2
  #skriv3
  #skriv4_html("turnering.html")
  #find_promotions
  #write_promotions("turnering.html")
  #delayed_promotions
  ##update-handicap-scores
  #write_table
  #show_players
  #show_all(date)
  #carl_johan() if carl_johan
  #count_nationality
end

def redo(year, from=1, to=72, berge=nil)
  #(cd (format nil "~d/t~d/" year from))
  #(when (= from 1)
  #  (format t "~%CP start->pre")
  #  (run-shell-command "cp ../ratingliste.start ./ratingliste.pre")
  #  )
  #(do ((next (1+ from) (1+ next)))
  #    ((> next to))
  #  (run :berger berger)
  #  (format t "~%CP post -> pre")
  #  (run-shell-command 
  #    (format nil "cp ./ratingliste.post ../t~d/ratingliste.pre" next))
  #  (cd (format nil "../t~d/" next))
  #  )
  #(cd "../../")
end

def rdo(n, berger=nil, year=2012)
  #(redo year :from n :to (1+ n) :berger berger))
end

def wiel(n, berger=nil, year='2010-wiel-2')
  #(redo year :from n :to (1+ n) :berger berger))
end

$e_limit = 18
$u_limit = 9

$rating = nil
$results = []
$all = []
$name = nil
$date = nil
$handicap = nil

def beste(liste, test='>', key='identity')
   return nil if liste.nil?

   beste = liste.first
   beste_verdi = beste.send(key)
   ny_best = nil

   liste[1..-1].each do |ny|
    ny_best = ny.send(key)
    if send(test, ny_best, beste_verdi)
      beste = ny
      beste_verdi = ny_best
    end
   end

   beste
end

def les(filename)
  File.open(filename, 'r') do |f|
    nr = 0
    started = false
    ended = false
    results = []
    nr_2 = nil
    $name = f.gets[1..-2] 
    $date = f.gets[1..-2]

    while (line = f.gets) != nil
      nr += 1
      results.unshift([nr] + parse_line(line))
    end

    $results = results.reverse
  end
end

def parse_line(line)
  macmahon = 0
  supplements = nil

  line.strip!
  match = line.match(/\d+ \[(.*?)\] \[(.*?)\] (.*?)\[(.*?)\]/)
  raise "Unable to parse in #{line}" unless match
  last_name = match[1]
  first_name = match[2]
  supplements = match[3]
  results = match[4]

  match = line.match(/\d+ \[(.*?)\] \[(.*?)\] (.*?)\[(.*?)\] \[(.*?)\]/)
  macmahon = match[5].to_i if match != nil

  [last_name, first_name, supplements, macmahon] + splitt_results(results)
end

def splitt_results(line)
  return :simultan if line.strip == '&'

  items = line.split(' ')
  results = items.map do |item|
    match = item.strip.match(/(\d+)([+-=])/)
    raise "unable to parse result #{item} at line #{line}" unless match
    result = [match[1].to_i, match[2]]
    match = item.strip.match(/\((.+)\)/)
    if match != nil
      result.unshift match[1]
    end
    result
  end
  results.reverse
end

class Player
  attr_accessor :last_name,
                :first_name,
                :nationality_list,
                :grade_level,
                :grade_name,
                :nsr_grade_level,
                :nsr_grade_name,
                :elo_number,
                :games,
                :last_played,
                :lb_count,
                :mp_count,
                :bonus_count

  def initialize
    @last_name = ''
    @first_name = ''
    @grade_level = 0
    @grade_name = ''
    @nsr_grade_level = 0
    @nsr_grade_name = ''
    @lb_count = nil
    @mp_count = nil
    @bonus_count = 0
  end
end

class Spiller
  attr_accessor :last_name,
                :first_name,
                :supplements,
                :macmahon,
                :result_rounds,
                :opponent_rounds,
                :handicap_side,
                :handicap_type,
                :clean_motstander_results,
                :u_p_k,
                :u_player,
                :simultan_p,
                :post_ant,
                :pts,
                :sos,
                :sb,
                :cum,
                :significance,
                :nr,
                :old_spiller,
                :rating_change
  def initialize
    @macmahon = 0
    @result_rounds = []
    @opponent_rounds = []
    @handicap_side = []
    @handicap_type = []
    @clean_motstander_results = []
    @u_p_k = 1
    @u_player = nil
    @simultan_p = nil
    @post_ant = 0
    @pts = 0
    @cum = 0
    @significance = nil
    @rating_change = 0
  end
end

class Home
  attr_accessor :list, #\E or #\A 多分ヨーロッパorアメリカ
                :nationality,
                :residens,
                :last
end

def spiller_navn(spiller)
  "#{spiller.last_name} #{spiller.first_name}"
end

$dummy = nil

def make_dummy(elo, u_p_k=1)
  player = Player.new
  player.elo_number = elo
  spiller = Spiller.new
  spiller.old_spiller = player
  spiller.u_p_k = u_p_k

  spiller
end

def pre_ant(spiller)
  value = (spiller.old_spiller.games)
  return value.size if value.is_a?(Array)

  value
end

def post_ant(spiller)
  spiller.post_ant
end

# post_ant(spiller)を展開した名前、antが引数のメソッド
#def 'post_ant(spiller)'(ant)
#  (setf (spiller-post-ant spiller) ant))
#  spiller.post_antを展開した名前の変数を作ってantを値にする
#end

def calculate
  all = $results.map do |line|
    spiller = Spiller.new
    spiller.last_name = line[1]
    spiller.first_name = line[2]
    spiller.supplements = line[3].strip
    spiller.nr = line[0]
    spiller.macmahon = line[4]
    spiller.pts = $inverted_macmahon ? 0 : line[4]
    spiller.cum = line[4] * (line[5..-1].size > 1 ? line[5..-1].size : 0)
    spiller.simultan_p = line[5] == :simultan

    spiller
  end

  motspiller = nil

  $dummy = Spiller.new
  $dummy.last_name = "DUMMY"
  $dummy.nr = 0
  $dummy.pts = 0
  all.unshift($dummy)

  all[1..-1].zip($results).each do |arr|
    spiller = arr[0]
    line = arr[1]
    next if line[5] == :simultan
    line[5..-1].each do |x|
      spiller.opponent_rounds.unshift(all[x[0]])
      res = case x[1]
      when '+'
        1.0
      when '-'
        0.0
      when '='
        0.5
      else
        raise "unexpected round result"
      end
      spiller.result_rounds.unshift(res)

      $handicap = true unless x[2].nil?
      spiller.handicap_side.unshift(x[2])
      spiller.handicap_type.unshift(x[3])
    end
  end

  all[1..-1].each do |spiller|
    next if spiller.simultan_p
    spiller.opponent_rounds.size.times do |round|
      motspiller = spiller.opponent_rounds[round]
      raise "Who did #{spiller.last_name} #{spiller.first_name} play against in round #{round+1}?" unless motspiller
      if motspiller == $dummy
        #noop
      elsif motspiller.simultan_p
        # TODO
      elsif motspiller.opponent_rounds[round] != spiller
        "Spillte #{spiller_navn(spiller)} og #{spiller_navn(motspiller)} mot hverandre i runde #{round+1}?"
      elsif (spiller.result_rounds[round] + motspiller.result_rounds[round]) != 1.0
        raise "#{spiller_navn(spiller)} og #{spiller_navn(motspiller)} fikke ikke et poeng tilsammen i runde #{round+1} (#{spiller.result_rounds} #{spiller.opponent_rounds.map(&:spiller_navn)} #{motspiller.result_rounds} #{motspiller.opponent_rounds.map(&:spiller_navn)})"
      else
        sjekk_handicap(round, spiller, motspiller)
      end
    end
  end

  all[1..-1].each do |spiller|
    spiller.pts += spiller.result_rounds.sum
  end

  all[1..-1].each do |spiller|
    spiller.sos = spiller.opponent_rounds.map(&:pts).sum
    spiller.sb = spiller.opponent_rounds.zip(spiller.result_rounds).map do |arr|
      sp = arr[0]
      res = arr[1]
      sp.pts * res
    end.sum
    sum = 0
    sum_sum = 0
    spiller.result_rounds.each do |res|
      sum += res
      sum_sum += sum
    end
    spiller.cum += sum_sum
  end

  all[1..-1].each do |spiller|
    spiller.clean_motstander_results = spiller.opponent_rounds.zip(spiller.result_rounds, spiller.handicap_side, spiller.handicap_type).delete_if {|e| e[0] == $dummy}
  end

  $all = all[1..-1]
end

def sjekk_handicap(round, spiller, motspiller)
  # handicapのvalidation? sjekk = check
  true
end

def find_origin
end

def change_us_name
end

def skriv1
end

def skriv2
end

def skriv2_html
end

def skriv3
end

def skriv3_html
end

def skriv4_html
end

def write_promotions
end

def reorder(berger)
end

def read_table(filnavn1="ratingliste.pre",filnavn2="nye-spillere")
  $rating = []
  File.open(filnavn1, 'r') do |f|
    while line = f.gets do
      next if line.strip.size == 0 || line.strip == ')'
      match = line.match(/#S\(PLAYER :LAST-NAME (.*?) :FIRST-NAME (.*?) :NATIONALITY-LIST \(#S\(HOME :LIST (.*?) :NATIONALITY (.*?) :RESIDENS (.*?) :LAST (.*?)\)\) :GRADE-LEVEL (.*?) :GRADE-NAME (.*?) :NSR-GRADE-LEVEL (.*?) :NSR-GRADE-NAME (.*?) :ELO-NUMBER (.*?) :GAMES (.*?) :LAST-PLAYED (.*?) :LB-COUNT (.*?) :MP-COUNT (.*?) :BONUS-COUNT (.*?)\)/)
      raise "Unable to parse player #{line}" unless match
      unquoted = match.to_a.map { |item| item.gsub(/"/,'') }

      player = Player.new
      player.last_name = unquoted[1]
      player.first_name = unquoted[2]
      home = Home.new
      home.list = unquoted[3]
      home.nationality = unquoted[4]
      home.residens = unquoted[5]
      home.last = unquoted[6]
      player.nationality_list = [home]
      player.grade_level = unquoted[7].to_i
      player.grade_name = unquoted[8]
      player.nsr_grade_level = unquoted[9].to_i
      player.nsr_grade_name = unquoted[10]
      player.elo_number = unquoted[11].to_i
      games = unquoted[12]
      if games[0] == '(' && games[-1] == ')'
        # gamesがリストの場合があるのでその場合はリストをそのまま入れる
        #:GAMES ((2160 0 1) (2160 1 1) (2167 1 1) (2185 1 1) (2041 1 1) (1910 1 1) (2292 1 1)) 
        # => 
        # [[2160, 0, 1], [2160, 1, 1], [2167, 1, 1], [2185, 1, 1], [2041, 1, 1], [1910, 1, 1], [2292, 1, 1]]
        player.games = games.gsub(/\(\(|\)\)/,'').split(') (').map { |game| game.split(' ').map(&:to_i) }
      else
        player.games = games.to_i
      end
      player.last_played = unquoted[13]
      player.lb_count = unquoted[14].to_i
      player.mp_count = unquoted[15].to_i
      player.bonus_count = unquoted[16].to_i
      $rating.unshift(player)
    end
  end
end

def write_table(filenavn="ratingliste.post")
end

def parse_new_player(list_player)
end

def find_player(last_name, first_name)
end

def find_players
  $all.each do |spiller|
    if new_old_spiller(spiller, Player.new)
      player = Player.new
      player.last_name = "Dummy"
      player.first_name = ""
      player.natinality_list = [Home.new]
      player.grade_level = 0
      player.grade_name = ""
      player.nsr_grade_level = 0
      player.nsr_grade_name = ""
      player.games = 0
      player.last_played = 0
      spiller.old_spiller = player
    elsif spiller.simultan_p
      player = Player.new
      player.last_name = "Dummy"
      player.first_name = ""
      player.natinality_list = [Home.new]
      player.grade_level = 0
      player.grade_name = ""
      player.nsr_grade_level = 0
      player.nsr_grade_name = ""
      player.games = 0
      player.last_played = 0
      spiller.old_spiller = player
    else
      player = $rating.find { |p| new_old_spiller(p, spiller) }
      raise "Unknown player : #{spiller.last_name} #{spiller.first_name} " unless player
      spiller.old_spiller = player
    end 
  end
  # TODO 日付のvalidation
end

def date_before(old_date, new_date)
end

def new_old_spiller(new, old)
  new.last_name == old.last_name && new.first_name == old.first_name
end

def innsats2(motstander_poeng_k_list, show=true)
  guess = motstander_poeng_k_list[0][0]
  limit = 0.001
  log10_40 = Math.log(10) / 400.0
  u1 = nil
  u2 = nil
  a = nil
  b = nil
  modification = nil

  loop do
    a = 0.0
    b = 0.0
    motstander_poeng_k_list.each do |m_p_k|
      u1 = 10 ** ((m_p_k[0] - guess) / 400.0)
      u2 = 1.0 / (1.0 + u1)
      a += m_p_k[2] * (m_p_k[1] - u2)
      b += m_p_k[2] * u1 * u2 * u2
    end
    modification = a / b / log10_40
    if modification > 800.0
      modification = 800.0
    elsif modification < -800.0
      modification = -800.0
    end

    limit += 0.0001
    if modification.abs < limit
      if show
        print "#{motstander_poeng_k_list} -> #{(guess + modification).round} +/- #{limit}"
      end
      return [($old_rule ? 400 : 1), (guess + modification).round].max
    end

    guess += modification
  end
end

def plus_minus
  all_players
  inc_games
end

def all_players
  sumdiff = Float::INFINITY
  motstander_poenger_k = nil
  spiller_change = nil
  pre_ant = nil
  post_ant = nil
  runder = nil
  dummy_elo = nil
  beste = nil
  bonus_count = nil

  $all.each do |spiller|
    pre_ant = pre_ant(spiller)
    if pre_ant == 0
      if spiller.old_spiller.nsr_grade_level > 0     
        dummy_elo = make_dummy(grade_elo(spiller.old_spiller.nsr_grade_level, spiller.old_spiller.nsr_grade_name))
        spiller.clean_motstander_results.unshift([dummy_elo, 1, [], []])
        spiller.clean_motstander_results.unshift([dummy_elo, 0, [], []])
      end
      spiller.post_ant = spiller.clean_motstander_results.size
      spiller.u_player = true
      spiller.old_spiller.elo_number = 0
    elsif spiller.old_spiller.games.is_a?(Numeric)
      spiller.post_ant = pre_ant + spiller.clean_motstander_results.size
    elsif spiller.old_spiller.games.all?{ |game| game[1] == 0 } or
            spiller.old_spiller.games.all?{ |game| game[1] == 1 } or
            spiller.old_spiller.elo_number == 0 or
            pre_ant + spiller.clean_motstander_results.size < $u_limit
      spiller.old_spiller.games.each do |old_result|
        spiller.clean_motstander_results.unshift([make_dummy(old_result[0],old_result[2]), old_result[1], [], []])
      end
      spiller.post_ant = spiller.clean_motstander_results.length
      spiller.u_player = true
      spiller.old_spiller.elo_number = 0
    else
      spiller.post_ant = pre_ant + spiller.clean_motstander_results.size
    end
  end

  loop do
    return if sumdiff == 0
    print "\n#{sumdiff} -> Iterate"
    sumdiff = 0
    $acc_weak_bonus = 0
    $acc_uppset_gain = 0

    $all.each do |spiller|
      motstander_poenger_k = get_elo_results(spiller)
      print("\n#{spiller.last_name} #{spiller.first_name} (#{spiller.old_spiller.elo_number})")
      spiller_change = 0
      pre_ant = pre_ant(spiller)

      if !spiller.u_player
        bonus_count = spiller.old_spiller.bonus_count
        motstander_poenger_k.each do |motstander_poeng_k|
          if $normal_output
            d = diff(spiller.old_spiller.elo_number + spiller_change, motstander_poeng_k[0], motstander_poeng_k[1], motstander_poeng_k[2], bonus_count, false)
            print sprintf("%+.2f",d)
          end
          spiller_change += diff(spiller.old_spiller.elo_number + spiller_change, motstander_poeng_k[0], motstander_poeng_k[1], motstander_poeng_k[2], bonus_count, true)
          bonus_count += 1
        end

        spiller_change = spiller_change.round
        print "= #{spiller_change}"
        runder = spiller.clean_motstander_results.size
        print " -> #{spiller_change + spiller.old_spiller.elo_number}"
      elsif spiller.clean_motstander_results.all? { |x| x[1] == 0 }
        print " 0% -> 1"
        spiller_change = $old_rule ? 400 : 1
      elsif spiller.clean_motstander_results.all? { |x| x[1] == 1 }
        beste = beste(motstander_poenger_k, '>', 'first').dup
        beste[1] = 0.5
        spiller_change = innsats2([beste] + motstander_poenger_k)
      else
        spiller_change = innsats2(motstander_poenger_k)
      end

      sumdiff += kvad(spiller.rating_change - spiller_change)
      spiller.rating_change = spiller_change
    end
  end
end

def elo_k(elo)
  v = if elo >= lb(5, 'Dan')
    16
  elsif elo >= lb(3, 'Dan')
    20
  elsif elo >= lb(1, 'Kyu')
    24
  elsif elo >= lb(4, 'Kyu')
    28
  elsif elo >= lb(7, 'Kyu')
    32
  elsif elo >= lb(11, 'Kyu')
    36
  else
    40
  end

  $importance * v
end

def diff(my, opponent, result, modification=1, bonus_count=200, for_update=false)
  k = modification * elo_k(my)
  normal_gain = k * (result - (1.0 / (1.0 + (10 ** ((opponent - my) / 400.0)))))
  upset_gain = (k * (opponent - my)) / 160.0
  weak_bonus = (bonus_count < 100 && my < 1800) ? (1800.0 - my) / 200.0 : 0.0

  if upset_gain > normal_gain && opponent > my && result == 1
    if for_update
      $acc_uppset_gain += (upset_gain - normal_gain)
    end
    if $normal_output
      print "*"
    end
  end

  if for_update
    $acc_weak_bonus += weak_bonus
  end

  (result == 1 && opponent > my && upset_gain > normal_gain ? upset_gain : normal_gain) + weak_bonus
end

def kvad(x)
  x * x
end

$dan_mp = [0,1740,1860,2000,2160,2340,2540,2760,3000,9999,9999]
$dan_lb = [0,1680,1800,1920,2080,2240,2440,2640,2880,3120,9999]

$kyu_mp = [0,1620,1510,1410,1320,1240,1160,1080,1000,920,840,760,680,600,520,440,360,280,200,120,40]
$kyu_lb = [0,1560,1460,1360,1280,1200,1120,1040, 960,880,800,720,640,560,480,400,320,240,160, 80, 0]

$dan_lb_number = [0,14,14,16,16,9999,9999,9999]
$dan_mp_number = [0, 7, 7, 8, 8,  10,9999,9999]

$kyu_lb_number = [0,14,12,12,12,10,10,10,8,8,8,8,6,6,6,6,6,6,6,6,6]
$kyu_mp_number = [0, 7, 6, 6, 6, 5, 5, 5,4,4,4,4,3,3,3,3,3,3,3,3,3]
                                
def grade_elo(value, type)
  if type == 'Dan'
    $dan_mp[value]
  elsif type == 'Kyu'
    $kyu_mp[value]
  elsif type == 'Pro'
    (2540 - 30) + (value * 30)
  else
    raise "Unknown rating type : #{type}"
  end
end

def lb(value, type)
  case type
  when 'Pro'
    9999
  when 'Dan'
    $dan_lb[value]
  when 'Kyu'
    $kyu_lb[value]
  else
    raise "Unknown grade #{value} #{type}"
  end
end

def mp(value, type)
  case type
  when 'Pro'
    9999
  when 'Dan'
    $dan_mp[value]
  when 'Kyu'
    $kyu_mp[value]
  else
    raise "Unknown grade #{value} #{type}"
  end
end

def get_elo_results(spiller)
  $old_rule ? get_elo_results_old(spiller) : get_elo_results_new(spiller)
end

def get_elo_results_new(spiller)
  spiller.clean_motstander_results.map { |result_list| get_single_elo_result_new(spiller, result_list) }
end

def get_start_rating(spiller)
  old = spiller.old_spiller.elo_number
  old == 0 ? spiller.rating_change : old
end

def clean_rating_change(spiller)
  start = spiller.old_spiller.elo_number
  start == 0 ? 0 : spiller.rating_change
end

$m_handicap_r  = 0.20
$l_handicap_r  = 0.60
$b_handicap_r  = 1.50
$r_handicap_r  = 2.10
$rl_handicap_r = 2.70
$two_p_handicap_r = 3.60
$four_p_handicap_r = 5.00
$five_p_handicap_r = 6.50
$six_p_handicap_r = 8.00
$s_handicap_r  = 1

def get_single_elo_result_new(spiller, result_list)
  my_rating = get_start_rating(spiller)
  rating_change = clean_rating_change(result_list[0])
  opponent_rating = [400, get_start_rating(result_list[0]) + rating_change].max
  modification = if result_list[3].nil? || result_list[3].size == 0
    0
  elsif result_list[3] == "S" 
    $m_handicap_r
  elsif result_list[3] == "M"
    $m_handicap_r
  elsif result_list[3] == "L"
    $l_handicap_r
  elsif result_list[3] == "B"
    $b_handicap_r
  elsif result_list[3] == "R" 
    $r_handicap_r
  elsif result_list[3] == "RL" 
    $rl_handicap_r
  elsif result_list[3] == "2p"
    $two_p_handicap_r
  elsif result_list[3] == "4p"
    $four_p_handicap_r
  elsif result_list[3] == "5p"
    $five_p_handicap_r
  elsif result_list[3] == "6p"
    $six_p_handicap_r
  else
    raise "Unknown handicap : #{result_list[3]}"
  end
  effective_opponent_rating = case result_list[4]
  when '+'
    ranknr_elo(elo-ranknr(opponent_rating) - modification)
  when '-'
    my_rating - (ranknr_elo(elo_ranknr(my_rating) - modification))
  else
    opponent_rating
  end
  [effective_opponent_rating, result_list[1], 1]
end

def inc_games
end

def elo_ranknr(elo)
  if elo > $dan_lb[1]
    main_nr = $dan_lb[1..-1].index { |r| r > elo }
    14.0 + main_nr + part_of(elo, $dan_lb[main_nr], $dan_lb[main_nr + 1])
  elsif elo > kyu_lb[1]
    14.0 + part_of(elo, $kyu_lb[1], $dan_lb[1])
  else 
    main_nr = $kyu_lb[1..-1].index { |r| r < elo }
    main_nr ? 14.0 - main_nr + part_of(elo, $kyu_lb[main_nr + 1], $kyu_lb[main_nr]) : 0.0
  end
end

def part_of(x, lb, ub)
  (x - lb) / (ub - lb)
end

def ranknr_elo(nr)
  main_nr = nr.floor
  part = (nr - main_nr).abs
  if main_nr >= 15
    value_of(part, $dan_lb[main_nr - 14], $dan_lb[main_nr - 13])
  elsif main_nr == 14
    value_of(part, $kyu_lb[1], $dan_lb[1])
  elsif main_nr >= -5
    value_of(part, $kyu_lb[15 - main_nr], $kyu_lb[14 - main_nr])
  else
    1
  end.round
end

def value_of(part, lb, ub)
  lb + (part * (ub - lb))
end

run
