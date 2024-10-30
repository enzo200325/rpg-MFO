module game_strategy {
    import basicSpells.* from "./spells/basicSpells"
    type Class = Mage | Druid | Hunter | Monster
    type Player = {
        player_class: Class, 
        hp: int, 
        initiative: int, 
        paralyzed: bool, 
        blinded: bool, 
        backup_hp: int, 
        transformed: bool, 
        illusion_on: bool, 
    }
    var turn_number: int 
    var current_player_index: int 
    var players_list: List[Player] 
    val number_of_players: int = 4 

    type Magic_Effect = Paralyze | Blind | Transform | Illusion | Deparalyze
    type Move = 
        | Attack({attacker: Player, receiver: Player, damage: int})
        | Magic({caster: Player, effect: Magic_Effect})
        | Skip({player: Player})
        | Attack_Sheep({attacker: Player})

    var last_move: Option[List[Move]]

    pure def change_hp(receiver: Player, change: int): Player = {
        val new_hp: int = receiver.hp + change
        if (new_hp <= 0 and receiver.player_class == Druid) {
            { ...receiver, hp: receiver.backup_hp, backup_hp: 0, transformed: false}
        }
        else { ...receiver, hp: new_hp}
    }
    
    pure def change_paralyze(receiver: Player): Player = {
        { ...receiver, paralyzed: not(receiver.paralyzed)}
    }

    pure def change_blind(receiver: Player): Player = {
        { ...receiver, blinded: not(receiver.blinded)}
    }

    pure def detransform_druid(druid: Player): Player = {
        if (druid.transformed) { ...druid, transformed: false, hp: druid.backup_hp, backup_hp: 0}
        else druid 
    }
    pure def transform_druid(druid: Player): Player = {
        if (druid.transformed == false) { ...druid, transformed: true, backup_hp: druid.hp, hp: 60}
        else druid 
    }

    pure def change_illusion(caster: Player): Player = {  
        { ...caster, illusion_on: true} 
    } 

    pure def ordering(p1: Player, p2: Player): bool = {
        p1.initiative > p2.initiative 
    }

    def monster_can_attack(new_players_list: List[Player]): bool = all {
        val monster_list: List[Player] = new_players_list.foldl([], (bl, p) => if (p.player_class == Monster) bl.append(p) else bl)
        val monster_set: Set[Player] = monster_list.indices().map(i => monster_list[i])
        nondet monster: Player = oneOf(monster_set)
        monster.blinded == false, 
        val sheep_counter: int = new_players_list.foldl(0, (sum, p) => if (p.illusion_on) sum + 1 else sum) 
        sheep_counter == 0,
        val bear_counter: int = new_players_list.foldl(0, (sum, p) => if (p.transformed) sum + 1 else sum) 
        bear_counter == 0
    }

    def someone_is_paralyzed(new_players_list: List[Player]): bool = all {
        val paralyzed_counter: int = new_players_list.foldl(0, (sum, p) => if (p.paralyzed) sum + 1 else sum)
        paralyzed_counter > 0
    }

    def can_attack(attacker_index: int, receiver_index: int): bool = all {
        val attacker: Player = players_list[attacker_index] all {
            attacker.hp > 0, 
            attacker.paralyzed == false, 
            attacker.blinded == false, 

            val receiver: Player = players_list[receiver_index] all {
                receiver.hp > 0, 
                attacker.player_class == Monster and receiver.player_class != Monster or attacker.player_class != Monster and receiver.player_class == Monster
            }
        },  
    }

    action deal_damage(attacker_index: int, receiver_index: int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index]
        val damage = match attacker.player_class {
            | Monster => if (turn_number == 0) 10 else if (attacker.blinded) 0 else 20
            | _ => 10
        } all {
            players_list' = new_players_list.replaceAt(receiver_index, change_hp(new_players_list[receiver_index], -damage)), 
            last_move' = Some([Attack({ attacker: new_players_list[attacker_index], receiver: new_players_list[receiver_index], damage: damage })])
        } 
    }

    action attack_player(attacker_index: int, receiver_index: int, new_players_list: List[Player]): bool = all {
        val sheep_counter: int = new_players_list.foldl(0, (sum, p) => if (p.illusion_on) sum + 1 else sum) 
        sheep_counter == 0 or new_players_list[attacker_index].player_class != Monster, 
        val bear_counter: int = new_players_list.foldl(0, (sum, p) => if (p.transformed) sum + 1 else sum) 
        bear_counter == 0 or new_players_list[attacker_index].player_class != Monster, 

        // se player && monster_can_attack
        new_players_list[attacker_index].player_class == Monster or not(someone_is_paralyzed(new_players_list)), 
        new_players_list[attacker_index].player_class == Monster or not(monster_can_attack(new_players_list)), 
        deal_damage(attacker_index, receiver_index, new_players_list),
    }
    
    action attack_sheep(attacker_index: int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index]
        attacker.player_class == Monster, 

        val sheep_index_list = 0.to(number_of_players - 1).filter(index => new_players_list[index].illusion_on) all {
            val sheep_counter: int = new_players_list.foldl(0, (sum, p) => if (p.illusion_on) sum + 1 else sum)
            sheep_counter > 0, 
            nondet mage_index = oneOf(sheep_index_list)
            players_list' = new_players_list.replaceAt(mage_index, { ...new_players_list[mage_index], illusion_on: false }), 
            last_move' = Some([Attack_Sheep({ attacker: new_players_list[attacker_index] })])
        }
    }

    action attack_bear(attacker_index: int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index]
        attacker.player_class == Monster, 

        val bears_index_list = 0.to(number_of_players - 1).filter(index => new_players_list[index].transformed) all {
            val bear_counter: int = new_players_list.foldl(0, (sum, p) => if (p.transformed) sum + 1 else sum) 
            bear_counter > 0,
            nondet druid_index = oneOf(bears_index_list)
            deal_damage(attacker_index, druid_index, new_players_list) 
        }
    }

    action bear_transform(attacker_index: int, new_players_list: List[Player]): bool = all {
        not(monster_can_attack(new_players_list)), 
        not(new_players_list[attacker_index].player_class != Monster and someone_is_paralyzed(new_players_list)), 
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.player_class == Druid, 
            attacker.transformed == false, 
            attacker.hp > 0, 
        }, 
        players_list' = new_players_list.replaceAt(attacker_index, transform_druid(new_players_list[attacker_index])), 
        last_move' = Some([Magic({ caster: new_players_list[attacker_index], effect: Transform })])
    }

    action create_illusion(attacker_index: int, new_players_list: List[Player]): bool = all {
        not(monster_can_attack(new_players_list)), 
        not(new_players_list[attacker_index].player_class != Monster and someone_is_paralyzed(new_players_list)), 
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.player_class == Mage, 
            attacker.illusion_on == false, 
            attacker.hp > 0, 
        }, 
        players_list' = new_players_list.replaceAt(attacker_index, change_illusion(new_players_list[attacker_index])), 
        last_move' = Some([Magic({ caster: new_players_list[attacker_index], effect: Illusion})])
    }

    action remove_paralysis(player_index: int, allies_index: Set[int], new_players_list: List[Player]): bool = all {
        new_players_list[player_index].player_class != Monster, 
        val paralyzed_index = allies_index.filter(p => new_players_list[p].paralyzed and new_players_list[p].hp > 0) all {
            paralyzed_index.size() > 0, 
            nondet receiver_index = oneOf(paralyzed_index)
            players_list' = new_players_list.replaceAt(receiver_index, change_paralyze(new_players_list[receiver_index])), 
        }, 
        last_move' = Some([Magic({ caster: new_players_list[player_index], effect: Deparalyze})])
    }

    action paralyze_player(attacker_index: int, receiver_index: int, new_players_list: List[Player]): bool = all {
        
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.player_class == Monster,
            attacker.hp > 0,
            attacker.blinded == false,
        }, 
        val receiver: Player = new_players_list[receiver_index] all {
            receiver.hp > 0,
            receiver.paralyzed == false,
        }, 
        players_list' = new_players_list.replaceAt(receiver_index, change_paralyze(new_players_list[receiver_index])),
        last_move' = Some([Magic({ caster: new_players_list[attacker_index], effect: Paralyze})])
    }

    action blind_monster(attacker_index: int, receiver_index: int, new_players_list: List[Player]): bool = all {
        not(monster_can_attack(new_players_list)), 
        not(new_players_list[attacker_index].player_class != Monster and someone_is_paralyzed(new_players_list)), 
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.hp > 0,
            attacker.player_class == Hunter,
        }, 
        val receiver: Player = new_players_list[receiver_index] all {
            receiver.hp > 0,
            receiver.blinded == false,
        }, 
        players_list' = new_players_list.replaceAt(receiver_index, change_blind(new_players_list[receiver_index])), 
        last_move' = Some([Magic({ caster: new_players_list[attacker_index], effect: Blind})])
    }

    action skip_turn(attacker_index : int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.hp <= 0,
            attacker.player_class != Monster and attacker.paralyzed,
        },
        players_list' = new_players_list, 
        last_move' = Some([Skip({ player: new_players_list[attacker_index]})])
    }

    def lose_modifiers(player: Player): Player = {
        match player.player_class {
            | Mage => { ...player, illusion_on: false }
            | Druid => detransform_druid(player)
            | Monster => { ...player, blinded: false }
            | _ => player
        }
    }

    action init = all {
        nondet player_1_initiative = oneOf(1.to(20))
        nondet player_2_initiative = oneOf(1.to(20))
        nondet player_3_initiative = oneOf(1.to(20))
        nondet player_4_initiative = oneOf(1.to(20))

        val player_1 = { player_class: Mage, hp: 20, initiative: player_1_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        val player_2 = { player_class: Hunter, hp: 20, initiative: player_2_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        val player_3 = { player_class: Druid, hp: 20, initiative: player_3_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false,}
        val player_4 = { player_class: Monster, hp: 100, initiative: player_4_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        
        // lista global com a ordem que os caras jogam 
        // ordered_players.append(player_1)

        players_list' = sortList(List(player_1, player_2, player_3, player_4), ordering), 
        turn_number' = 0,
        current_player_index' = 0,
        last_move' = None
    }

    action step = all {
        val new_players_list = players_list.foldl([], (l, p) => if (p == players_list[current_player_index]) l.append(lose_modifiers(players_list[current_player_index])) else l.append(p))
        val monsters_index_list = 0.to(number_of_players - 1).filter(index => players_list[index].player_class == Monster)
        val enemies_index = 0.to(number_of_players - 1).filter(index => can_attack(current_player_index, index))
        val allies_index = 0.to(number_of_players - 1).filter(index => not(can_attack(current_player_index, index)))

        nondet current_enemy_index = oneOf(enemies_index)
        any {
            attack_sheep(current_player_index, new_players_list),
            attack_bear(current_player_index, new_players_list),
            attack_player(current_player_index, current_enemy_index, new_players_list), // atacar monstro ou personagem (nao transformado)
            bear_transform(current_player_index, new_players_list),
            create_illusion(current_player_index, new_players_list),
            remove_paralysis(current_player_index, allies_index, new_players_list),
            paralyze_player(current_player_index, current_enemy_index, new_players_list),
            blind_monster(current_player_index, current_enemy_index, new_players_list),
            skip_turn(current_player_index, new_players_list),
        },

        turn_number' = turn_number + 1, 
        current_player_index' = (current_player_index + 1) % number_of_players
    }

    val monster_alive = players_list.foldl( true,(acc, i) => if(match i.player_class {
		| Monster => true
		| _ => false
		}) acc and i.hp>0 else acc )

	val players_alive = players_list.foldl( true,(acc, i) => if(match i.player_class {
		| Monster => false 
		| _ => true 
		}) acc and i.hp>0 else acc )


}