#Задача 1: 
#           В кутия има 8 топки, номерирани от 1 до 8. Вадим произволна топка и я връщаме в кутията. 
#           Отново вадим произволна топка. Каква е вероятността да извадим два пъти една и съща топка?

#balls <- 1:8
# balls <- c(1:8) ili c(1,2,3,4,5,6,7,8)

# Използвани команди: c, X:N, sample, any, which, <- function(){}; replicate
# sum, length, rep, duplicated, 

sim_balls <- function(max_balls_count) {
  balls <- 1:max_balls_count
  chosen <- sample(balls, 2, replace=T)
  chosen[1] == chosen[2]
}

prob_balls <- function(max_balls_count, tries) {
  # NB - how the passing of a function with params is made -> we invoke it!
  res <- replicate(tries, sim_balls(max_balls_count))
  sum(res) / length(res)
}

prob_balls(8, 1000000)
# 1/8 * 1/8 ? => Не, понеже вторият опит и първият са независими, т.е 1/8 само


# Задача 2: В кутия има 3 различни чифта чорапи. Вадим в тъмното 2 чорапа. Каква е вероятността извадените два чорапа да са чифт?

sim_socks <- function() {
  socks <- rep(c(1,2,3), 2)
  chosen <- sample(socks, 2, replace=F)
  chosen[1] == chosen[2]
}

#NB - when passing a function to replicate it needs to be invoked, as replicate
# operates on expressions and does not invoke the func itself!!!
prob_socks <- function(times) {
  res <- replicate(times, sim_socks())
  sum(res) / length(res)
}

prob_socks(100000)
# 1/6 * 1/5 + 1/6 * 1/5 + 1/6 * 1/5 ? - Еми не, важно ни е вторият, т.е при изтеглен първи е все тая - дали втория прави match, т.е 3 пъти 1/5 или => НЕ
# 1/30 + 1/30 + 1/30 = 3/30 = 1/10 -> Проблем? Емпирично е 1/5. Това е така, понеже не трябва да броим наредените n/орки, а само комбинациите, т.е /2! и отговорът
# ще излезе
# Имаме 6 choose 2 да са всички, а желани имам 3 вариатна или 3 / (6!/4!*2!) = 3 / 6*5/2 = 1/5 OK!


# Задача 3: Иван има 4 ключа, но не знае кой е за неговата стая. Предполагаме, че ключовете са разбъркани по произволен начин. 
#           Иван пробва последователно с всеки от тях, като помни кой ключ е пробвал. 
#           Каква е вероятността да отключи с последния (четвъртия) ключ?

# 3/4 * 2/3 * 1/2 = 6/24 = 1/4, което е и логично, понеже правилният да е на последен слот е 1/4 вероятността
sim_keys <- function(number_of_keys) {
  keys <- c(1:number_of_keys) # 4 -> 1,2,3,4
  order <- sample(keys, number_of_keys, replace = FALSE)
  order[number_of_keys] == 1 # Ако ключ 1 отваря вратата и е на позиция 4, то е отворил с последния ключ вратата
}

prob_keys <- function(times, number_of_keys = 4) {
  res <- replicate(times, sim_keys(number_of_keys))
  sum(res) / length(res)
}
# Забележи подаването на стойност по подразбиране!
# Грешка Error in sim_keys(number_of_keys) : object 'number_Of_keys' not found ппц означава typo в кода или проблем с областта на видимост
prob_keys(100000)


# Задача 4 
#           Студент се явява на изпит с конспект от 20 въпроса. От тях не знае само 3 въпроса. На изпита си тегли 2 въпроса от конспекта. 
#           Каква е вероятността да знае само един от изтеглените въпроси?

sim_questions <- function() {
  questions <- c(rep(1, 17), rep(0, 3))
  drawn <- sample(questions, 2, replace = FALSE)
  sum(drawn) == 1
}

prob_questions <- function(times) {
  res <- replicate(times, sim_questions())
  sum(res) / length(res)
}

prob_questions(10000000)
# 17/20 * 3/19 * 2! (къде е успехът) = 51 * 2 / 380


# Задача 5:
#           Каква е вероятността в група от 25 човека поне двама да имат рожден ден на един и същи ден от годината?

sim_birthday <- function(people) {
  days <- c(1:365)
  birthdays <- sample(days, people, replace = TRUE)
  any(duplicated(birthdays)) == 1
}

prob_birthday <- function(times, people) {
  res <- replicate(times, sim_birthday(people))
  sum(res) / length(res)
}

prob_birthday(100000, 25)


# Задача 6:
#           В отдел на фирма работят 20 човека. За Коледа те решават да си разменят подаръци. В кутия слагат 20 листчета, на всяко от които 
#           има едно име. Всеки тегли листче (без да го връща) и подарява на този, чието име е изтеглил. Каква е вероятността поне един да изтегли своето име?

sim_messages <- function(people_count) {
  # Един вариант е да се направи разбъркване с sample и да се проверява дали elem[i] == i и ако да да се приключи работа и върне истина
  people <- c(1:people_count)
  sent_letters <- sample(people, people_count, replace = FALSE)
  i <- 1
  while (i <= people_count) {
    if (sent_letters[i] == i)
      return(1)
    i <- i + 1
  }
  return(0)
}

# Алтернатива на горното е да се направи трети вектор разлика (a_vec - b_vec) и да се приравни с any(x==0), че да се открие дали имаме съвпадение
prob_messages <- function(times, people_count) {
  res <- replicate(times, sim_messages(people_count))
  sum(res) / length(res)
}

prob_messages(100000, 20)
# 1 - P(I am not myself) = 1 - (19/20*18/19*17/18*...*1/2) = 1 - (1/20) -> Не

# ? Може ли така да се реши?

# Задача 7:
#           На всеки от върховете на равностранен триъгълник има една мравка. Всяка мравка избира произволно един от другите два върха и тръгва към него. 
#           За единица време всяка мравка изминава разстоянието от един връх до друг. Две мравки могат да се разминат
#           ако тръгнат една срещу друга. Каква е вероятността след единица време да има по една мравка на всеки връх?

ant_move <- function(start_pos, ants_playing_count) {
  possible_moves <- c(1:ants_playing_count)
  
  # 2 вариант за премахване на елемент (NB - създава се и при трите ново копие, ще ни трябва присвояване)
  # Index based removal - ползваме отрицателни индекси, например arr[-2] създава нов вектор, като премахва елемента на индекс номер 2
  # Условието x!=6 създава вектор, който на всеки елемент от началния мапва TRUE или FALSE. x[(TRUE,FALSE,FALSE)] ще добави само първия елемент, тъй като взима неговия индекс!
  # Value based removal (същото е и логически removal) - Използва логическо индексиране arr[arr != 6] ще създаде нов вектор от arr, като условието е всеки елемент(стойност), която е != 6 ще остане
  possible_moves <- possible_moves[ -1 * start_pos ] # Така се премахват елементи от вектор (виж също setdiff). Идеята е да премахнем текущо-заемания от списъка
  sample(possible_moves, 1, replace = FALSE)
}

# За проба с премахването на елементи как работи
#ant_move(1, 3)
#x <- c(4,5,6,7)
#res <- x[-2]
#diff <- x==2
#secondVarDiff <- x[x != 6]

# Трябва да се оцени като истина или лъжа
sim_ants <- function(ants_playing_count) {
  i <- 1
  ant_directions <- vector()
  while (i <= ants_playing_count) {
    ant_directions <- c(ant_directions, ant_move(i, ants_playing_count))
    i <- i + 1
  }
  #!any(duplicated(ant_directions)) прави същото като долното
  length(unique(ant_directions)) == length(ant_directions)
}
# 1/2 * 1/2 * 1/2 + 1/2 * 1/2 * 1/2 = 2 * 1/8 = 1/4

prob_ants <- function(times, ants_playing_count) {
  res <- replicate(times, sim_ants(ants_playing_count))
  sum(res) / length(res)
}

prob_ants(100000, 3) # ~0.25 -> не е ли 1/4, тъй като само в 2 от 8 случая (2^3) ще имаме правилна конфигурация


# Задача 8:
#           В кутия има 6 сурови и 2 сварени яйца. Двама играчи, редувайки се, избират яйце докато извадят всички яйца. 
#           Намерете вероятностите на следните събития:
#           𝐴 = {на един играч се падат двете сварени яйца};
#           𝐵 = {пада се по едно сварено яйце на всеки играч};
#           𝐶 = {падат се двете сварени яйца на този, който тегли първи};
#           𝐷 = {падат се двете сварени яйца на този, който тегли втори}. 

draw_eggs <- function(raw, boiled) {
  eggs <- c(rep('b', boiled), rep('r', raw))
  sample(eggs, raw + boiled, replace = FALSE)
}

sum_eggs_per_person <- function(is_even, egg_sample, max_eggs) {
  if (is_even)
    sum(egg_sample[seq(2, 8, 2)] == 'b')
  else
    sum(egg_sample[seq(1, 8, 2)] == 'b')
}

person_has_N_eggs <- function(boiled_count, N) {
  boiled_count == N
}

both_eggs_in_same_person <- function(first_person_eggs, second_person_eggs) {
  person_has_N_eggs(first_person_eggs, 2) | person_has_N_eggs(second_person_eggs, 2)
}

both_people_have_one_egg <- function(first_person_eggs) {
  person_has_N_eggs(first_person_eggs, 1) # Idea is we have drawn eggs, checking for first person is enough to solve the task
}
# Горната функция ни разиграва играта, така че напаснем извадката към някой от резултатите

sim_eggs <- function(boiled_count, raw_count) {
  x <- draw_eggs(6, 2)
  first_person_eggs <- sum_eggs_per_person(FALSE, x, 6 + 2)
  second_person_eggs <- sum_eggs_per_person(TRUE, x, 6 + 2)
  
  A <- both_eggs_in_same_person(first_person_eggs, second_person_eggs)
  B <- both_people_have_one_egg(first_person_eggs)
  C <- person_has_N_eggs(first_person_eggs, 2)
  D <- person_has_N_eggs(second_person_eggs, 2)
  c(A, B, C, D)
}

prob_eggs <- function(boiled_count, raw_count, N) {
  result <- replicate(N, sim_eggs(boiled_count, raw_count))
  A_prob <- sum(result[1,]) / N
  B_prob <- sum(result[2,]) / N
  C_prob <- sum(result[3,]) / N
  D_prob <- sum(result[4,]) / N
  c(A_prob, B_prob, C_prob, D_prob)
}

result <- prob_eggs(2, 6, 1000000)


#  Задача 9. На студенти е даден тест от 10 въпроса, всеки с по 4 възможни отговора, един от които е верен.
#            Иван се явява на теста без да е учил и огражда произволно отговори. 
#            Каква е вероятността да е отговорил вярно на поне 5 от въпросите?

sim_test <- function(wrong_answers_count, right_answers_count, questions_count, minimum_right_answers) {
  question <- c(FALSE, TRUE)
  possible_answers_count <- wrong_answers_count + right_answers_count
  answer_sheet <- sample(question, questions_count, replace=TRUE, prob=c(wrong_answers_count / possible_answers_count, right_answers_count / possible_answers_count))
  sum(answer_sheet) >= minimum_right_answers 
}

prob_test <- function(wrong_answers_count, right_answers_count, questions_count, minimum_right_answers, N) {
  result <- replicate(N, sim_test(wrong_answers_count, right_answers_count, questions_count, minimum_right_answers))
  sum(result) / N
}

prob_test(3, 1, 10, 5, 1000000) # Binomial p=1/4, q = 3/4, With k going from 5 to 10 p^k(5) * q^(n-k) (a.k.a q^(10-5)) * n over k (10 over 5) 



#Задача 10. Авиокомпания е продала 143 билета за самолет, в който има 138 пътнически места. 
#           Вероятността пътник да дойде навреме за полета си е 0.92. 
#           Нека приемем, че даден пътник идва навреме независимо от останалите пътниците.
#           а) Каква е вероятността да има място за всички пътници, които са дошли навреме? -> събитие А
#           б) Каква е вероятността да остане едно незаето пътническо място? -> събитие B

sim_plane_people <- function(person_coming_prob, N) {
  person_coming <- c(TRUE, FALSE)
  responses <- sample(person_coming, N, replace=TRUE, prob =c(person_coming_prob, 1 - person_coming_prob))
  sum(responses)
}

sim_plane <- function(person_coming_prob, people_count, seats) {
  people <- sim_plane_people(person_coming_prob, people_count)
  A <- people <= seats
  B <- seats - people == 1
  c(A, B) # means we will create a vector of 2 elements - it will be a col to our table, every param creates a row!
}

prob_plane <- function(person_coming_prob, people_count, seats, N) {
  result <- replicate(N, sim_plane(person_coming_prob, people_count, seats))
  A <- sum(result[1,]) / N # Вземи реда за събитие А и го раздели на общата бройка
  B <- sum(result[2,]) / N
  c(A, B)
}

prob_plane(0.92, 143, 138, 100000)


#Задача 11. В една кутия има 2 зелени и 2 червени топки. В друга кутия има 1 зелена и 4 червени топки. 
#           Хвърляме зар и ако се падне шестица, теглим топка от първата кутия, а ако не се падне шестица, теглим топка от втората кутия.
#           а) Каква е вероятността да извадим зелена топка?
#           б) Ако извадената топка е зелена, каква е вероятността да е извадена от втората кутия?

# !!! NB - R expects {} on the same row and control flows to ALWAYS have braces 
# (also ELSE is expected to be on the same line as }, so } else)

sim_conditional_balls <- function() {
  first_box <- rep(c('g', 'r'), 2)
  second_box <- c('g', rep('r', 4))
  dice_result <- sample(1:6, 1, replace=FALSE)
  if (dice_result == 6) {
    drawn_ball <- sample(first_box, 1, replace=FALSE)
  } else {
    drawn_ball <- sample(second_box, 1, replace=FALSE)
  }
  A <- drawn_ball == 'g'
  # За извадена топка зелена разглеждаме 3 случая - вадим зелена от първа, от втора, или не вадим зелена (не трябва да влияе на вероятността)
  # Не, нещо по-яко - не разглеждаме 3 случая, а ще филтрираме после резултатите. Абе не, със трите сценария е по-чисто
  B <- 2 # приемаме резултата за трета алтернатива (2), ако топката не е била зелена
  if (A) {
    B <- dice_result != 6 # Питаме се да е изтеглена от по-гадната, т.е втората кутия. За целта не трябва да се е паднала шестица
  }
  c(A, B)
}

prob_conditional_balls <- function(N) {
  result <- replicate(N, sim_conditional_balls())
  A <- sum(result[1,]) / N  # result[1, ] съдържа резултата на събитието, дали сме изтеглили зелена топка
  interesting_results <- result[2, result[2,] != 2]  # result [2, ] засяга събитието B - т.е всичките му резултати, но ти ни вълнува само ако сме изтеглили зелена топка (Бяхме маркирали B<-2, ако такава не се
  #                                                     е случила, тоест този сценарий да не се разглежда) 
  B <- sum(interesting_results) / length(interesting_results)
  c(A,B)
}

prob_conditional_balls(1000000)

test_a <- c(1,2)
test_b <- c(1,2)
test_a == test_b
all(test_a == test_b)

#Задача 12. Разглеждаме три типа монети: тип T11 имат изписана единица от двете страни, тип T22 имат двойка от двете страни и 
#           тип T12 имат единица от едната страна и двойка от другата. В кутия има две монети T11, една монета T22 и две монети T12. 
#           Теглим произволна монета и я хвърляме. а) Каква е вероятността да се падне единица?
#           б) Ако горната страна на хвърлената монета е единица, каква е вероятността другата страна да е двойка?

sim_coins <- function(t11_count, t22_count, t12_count) {
  t11 <- 11
  t22 <- 22
  t12 <- 12 # could be 21 but it is the same for our task (c(1,2), c(1,1), c(2,2) are invalid, because R unpackages them!)
  pool <- c(rep(t11, t11_count), rep(t22, t22_count), rep(t12, t12_count))
  drawn_coin <- sample(pool, 1, replace=FALSE) # draw a coin from the pool
  
  # throwing the coin is a bit tricky => modulo or integer division
  which_side <- sample(1:2, 1, replace=FALSE) # pick side of the coin
  face <- drawn_coin %% 10
  if (which_side == 1)
    face <- floor(drawn_coin / 10)

  A <- face == 1
  B <- 2
  if (A) {
    # проверка за двете страни 1-ца? Еми всеки елемент на монетата(вектора трябва да е единица) - иначе arr[arr == 1]
    
    # Ние тук ще се питаме дали е двойка -> ТОВА Е АКО БЯХМЕ С Вектори!
    ## * test_a <- c(1,2)
    #   test_b <- c(1,1)
    #   test_a == test_b -> сравнява arr1[i] == arr2[i] за i от 1 до N
    # all(drawn_coin == t12), ако е изпълнено за всяко, т.е монетите са еднакви 
    #B <- all(drawn_coin == t12) # забележи, че наредбата (1,2) ни е гарантирана за монетата, понеже не сме я завъртали
    
    B <- drawn_coin == t12
  }
  
  c(A,B)
}

# replicate func
prob_coins <- function(t11_count, t22_count, t12_count, N) {
  result <- replicate(N, sim_coins(t11_count, t22_count, t12_count))
  A <- sum(result[1,]) / N
  one_on_the_upside <- result[2, result[2,] != 2]
  B <- sum(one_on_the_upside) / length(one_on_the_upside)
  c(A,B)
}

prob_coins(2, 1, 2, 100000)

#  Задача 13. Имаме 3 карти: първата е бяла от двете страни, втората е черна от двете страни, 
#             а третата е бяла от едната и черна от другата страна. Всяка карта е поставена в затворена кутия. 
#             Избираме произволна кутия, отваряме я и виждаме, че горната страна на картата в нея е бяла. 
#             Каква е вероятността другата страна на картата също да е бяла?

# Аналогична на горната
sim_colourful_cards <- function(only_white_count, only_black_count, mixed_count) {
  # 'ww', 'bb', 'wb'
  whites <- rep('ww', only_white_count)
  #blacks <- rep('bb, only_black_count)
  mixed <- rep('wb' ,mixed_count)
  # Забележка - по условие пише, че горната страна е бяла, така че трябва да си актуализираме в.п-во, така че да няма черна/черна,
  # тъй като ни питат щом сме изтеглили горна бяла карта, долната да е също бяла?
  
  pool <- c(rep(whites, 2), mixed) # Да приемем, че виждаме страната с индекс 1 и знаем, че е бяла, тогава W = (б,б), (б,б), (б,ч)
  drawn_card <- sample(pool, 1, replace=FALSE)
  # Тегавиня, понеже не се знае ние на бяла, бяла коя бяла страна гледаме, т.е не е 1/2 вероятността само, а е 2/3
  # А ако решим обратния проблем? Да не сме в бяла, бяла, т.е 1 - бяла, черна? W = {(б1, б2), (б2, б1), (б, ч)}
  drawn_card == 'ww'
}

prob_colourful_cards <- function(N, only_white_count, only_black_count, mixed_count) {
  result <- replicate(N, sim_colourful_cards(only_white_count, only_black_count, mixed_count))
  sum(result) / N  
}

prob_colourful_cards(100000, 1, 1, 1)


#Задача 14. В кутия има 99 топки номерирани от 1 до 99. Теглим без връщане 4 случайно избрани топки. 
#           Каква е вероятността първата извадена топка да е с най-голям номер от извадените?
sim_numbered_balls <- function(start, end, balls_to_drawn) {
  drawn <- sample(start:end, balls_to_drawn, replace=FALSE)
  first_ball <- drawn[1]
  remaining <- drawn[-1]
  all(first_ball > remaining)
}

prob_numbered_balls <- function(N, start, end, balls_to_drawn) {
  result <- replicate(N, sim_numbered_balls(start, end, balls_to_drawn))
  sum(result) / N
}

prob_numbered_balls(100000, 1, 99, 4) # 0.25008, which is obvious - from 4 numbers, the first to be the greatest
# This is different from drawing an ordered sequence! C(N, 4)/V(N,4) = N!/(N-4)!*4! / N!/(N-4)! = 1/4!


# Задача 15. Група от 20 човека, измежду които са Иван и Георги, е подредена по случаен начин в редица. 
#            Каква е вероятността Иван и Георги да са един до друг?

sim_neighbours <- function(random_people_count) {
  # Вариант е да си ги мислим като група и вече нали 19!*2!/20! = 2!/20 = 1/10
  # Нека емпирично го решим
  
  # r as random
  people <- c(rep('r', random_people_count), 'i', 'g')
  config <- sample(people, random_people_count + 2, replace=FALSE)
  
  # Взимаме им индексите и проверяваме дали разликата по модул е == 1 (тогава са един до друг)
  # *Решение с 2 while цикъла за намиране на индексите ИЛИ
  # функция which -> връща индексите на всеки TRUE-ти елемент според условието. Алтернатива е функцията match
  
  # Логически вектор? Вектор съдържащ стойности TRUE или FALSE, затова трябва като първи аргумент да подадем проверка върху вектор
  georgi_index <- which(config == 'g')  # Ппц which връща масив от индекси, върху, чийто елементи проверката е върнала истина. Тук е гарантирано, че той винаги е само един
  ivan_index <- which(config == 'i')
  abs(georgi_index - ivan_index) == 1
}

prob_neighbours <- function(N, random_people_count) {
  result <- replicate(N, sim_neighbours(random_people_count))
  sum(result) / N
}

prob_neighbours(1000000, 18) # 1/10 или емпирично 0.100281


#Задача 16. Тесте от 52 карти е разбъркано и е раздадено на 4 играчи. Каква е вероятността всеки играч да има едно асо?

are_aces_of_person_equal_to_number <- function(deck_config, person_index, people_playing, total_cards, searched_aces_number) {
  # draws the hand of the person from the deck when all cards are cycled for all people (1,2,3,4,1,2,...)
  person_cards <- deck_config[seq(person_index, total_cards, people_playing)]
  person_aces <- sum(person_cards == 'A')
  person_aces == searched_aces_number
}

sim_aces <- function(people_playing, other_random_cards_count, searched_aces_number) {
  cards <- c(rep('A', people_playing), rep('R',other_random_cards_count))
  config <- sample(cards, other_random_cards_count + people_playing, replace=FALSE)
  
  # Може да се направи игра при която |асата| != |броя хора|, но не се пита това и без това в задачата
  has_each_person_matched_the_searched_count <- c(
    are_aces_of_person_equal_to_number(config, 1, people_playing, other_random_cards_count + people_playing, searched_aces_number),
    are_aces_of_person_equal_to_number(config, 2, people_playing, other_random_cards_count + people_playing, searched_aces_number),
    are_aces_of_person_equal_to_number(config, 3, people_playing, other_random_cards_count + people_playing, searched_aces_number),
    are_aces_of_person_equal_to_number(config, 4, people_playing, other_random_cards_count + people_playing, searched_aces_number)
  )
  
  #which(config == 'A') - for checking indexes
  sum(has_each_person_matched_the_searched_count) == people_playing
}

prob_aces <- function(N, people_playing, other_random_cards_count, searched_aces_number) {
  result <- replicate(N, sim_aces(people_playing, other_random_cards_count, searched_aces_number))
  sum(result) / N
}

prob_aces(100000, 4, 48, 1)


#Задача 17. На първия етаж на административна сграда 7 души чакат асансьора. Всеки от тях отива в някой от офисите в сградата. 
#           Сградата има 16 етажа и на всеки етаж има равен брой офиси (на първия етаж няма офиси).
#           а) Каква е вероятността поне двама от чакащите да отиват на един и същи етаж?
#           б) Ако Вие сте един от седемте, каква е вероятността поне един от останалите 6 да отива на Вашия етаж?


# Идеята на равен брой офиси и на първия етаж няма офиси е, че разпределението на офисите е равномерно, т.е P е равномерна
sim_levels <- function(people_count, floors) {
  # Ако трябва да го решаваме аналитично, това е birthday paradox, така че 1 - няма повторения(), няма повторения = V(15,7)/ 15^брой хора
  paths <- sample(2:floors, people_count, replace = TRUE)
  A <- any(duplicated(paths))
  
  # Б) Тук идеята е да фиксираме етажа си и да решим задачата за 6 души спрямо него. Това не е същото като а), 
  #     тъй като там се гледа всеки с всекиgo (вариация)
  # Все тая е дали ще кажем кой е етажа ни или ще го избираме случайно, важното е че после в извадката няма да е заедно (НАЛИ??) - питай
  # Абе така е, но долното е по-красиво тъй като участва в конфигурацията, а не правим втора
  
  # Би било грешно да теглим 2 пъти(не много де, понеже опитите са независими, но все пак), редно е да изследваме в една конфигурация
  searched_person_index = sample(1:people_count, 1)
  searched_person_floor = paths[searched_person_index]
  colleagues_paths <- paths[-1 * searched_person_index]
  
  B <- any(colleagues_paths == searched_person_floor)
  c(A,B)
}

prob_levels <- function(N, people_count, floors) {
  result <- replicate(N, sim_levels(people_count, floors))
  A <- sum(result[1,]) / N
  B <- sum(result[2,]) / N
  c(A,B)
}

prob_levels(100000, 7, 16)