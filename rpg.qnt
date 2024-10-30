module rpg {
    import basicSpells.* from "./spells/basicSpells"

    type Class = Mage | Druid | Hunter | Monster 
    type Player = {
        player_class: Class, 
        playable: bool, 
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
    var number_of_players: int

    type Modifier = Ilusion | Blindness | Transform

    pure def change_hp(receiver: Player, change: int): Player = {
        { ...receiver, hp: receiver.hp + change}
    }
    
    pure def change_paralyze(receiver: Player): Player = {
        { ...receiver, paralyzed: not(receiver.paralyzed)}
    }

    pure def change_blind(receiver: Player): Player = {
        { ...receiver, blinded: not(receiver.blinded)}
    }

    // BECAME "detransform_druid"
    // pure def change_detransform(receiver: Player): Player = {
    //     { ...receiver, transformed: false, hp: receiver.backup_hp, backup_hp: 0}
    // }

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
        p1.initiative < p2.initiative 
    }

    def can_attack(attacker_index: int, receiver_index: int): bool = all {
        val attacker: Player = players_list[attacker_index] all {
            attacker.hp > 0, 
            attacker.paralyzed == false, 
            attacker.blinded == false, 

            val receiver: Player = players_list[receiver_index] all {
                receiver.hp > 0, 
                attacker.player_class == Monster and receiver.player_class != Monster,
                attacker.player_class != Monster and receiver.player_class == Monster,
            }
        },  
    }

    action deal_damage(attacker_index: int, receiver_index: int, new_players_list: List[Player]): bool = {
        val attacker: Player = new_players_list[attacker_index]
        val damage = match attacker.player_class {
            | Monster => if (turn_number == 0) 10 else if (attacker.blinded) 0 else 20
            | _ => 10
        }
        all {
            players_list' = new_players_list.replaceAt(receiver_index, change_hp(new_players_list[receiver_index], -damage))
            // match receiver.player_class {
            //     | Druid => ordered_players' = new_players.foldl([], (acc, p) => acc.append(if (p.hp <= 0) change_detransform(p) else p))
            //     | _ => ordered_players' = new_players
            // }
        }
    }

    action attack_player(attacker_index: int, receiver_index: int, new_players_list: List[Player]): bool = all {
        val sheep_counter: int = new_players_list.foldl(0, (sum, p) => if (p.illusion_on) sum + 1 else sum) 
        sheep_counter == 0, 
        val bear_counter: int = new_players_list.foldl(0, (sum, p) => if (p.transformed) sum + 1 else sum) 
        bear_counter == 0, // verificando se nao tem bears (da certo?)
        deal_damage(attacker_index, receiver_index, new_players_list),
    }
    
    action attack_sheep(attacker_index: int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index]
        attacker.player_class == Monster, 

        val sheep_index_list = 0.to(number_of_players).filter(index => new_players_list[index].illusion_on) all {
            val sheep_counter: int = new_players_list.foldl(0, (sum, p) => if (p.illusion_on) sum + 1 else sum)
            sheep_counter > 0, 
            nondet mage_index = oneOf(sheep_index_list)
            players_list' = new_players_list.replaceAt(mage_index, { ...new_players_list[mage_index], illusion_on: false })
        }
    }

    action attack_bear(attacker_index: int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index]
        attacker.player_class == Monster, 

        val bears_index_list = 0.to(number_of_players).filter(index => new_players_list[index].transformed) all {
            val bear_counter: int = new_players_list.foldl(0, (sum, p) => if (p.transformed) sum + 1 else sum) 
            bear_counter > 0,
            nondet druid_index = oneOf(bears_index_list) all {
                // deal_damage(attacker_index, druid_index), 
                players_list' = if (new_players_list[druid_index].hp <= 0) new_players_list.replaceAt(druid_index, detransform_druid(new_players_list[druid_index])) else new_players_list 
            } 
        }
    }

    action bear_transform(attacker_index: int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.player_class == Druid, 
            attacker.transformed == false, 
            attacker.hp > 0, 
        }, 
        players_list' = new_players_list.replaceAt(attacker_index, transform_druid(new_players_list[attacker_index]))
    }

    action create_illusion(attacker_index: int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.player_class == Mage, 
            attacker.illusion_on == false, 
            attacker.hp > 0, 
        }, 
        players_list' = new_players_list.replaceAt(attacker_index, change_illusion(new_players_list[attacker_index]))
    }

    action remove_paralysis(player_index: int, allies_index: Set[int], new_players_list: List[Player]): bool = all {
        new_players_list[player_index].player_class != Monster, 
        val paralyzed_index = allies_index.filter(p => new_players_list[p].paralyzed and new_players_list[p].hp > 0)
        nondet receiver_index = oneOf(paralyzed_index)
        players_list' = new_players_list.replaceAt(receiver_index, change_paralyze(new_players_list[receiver_index]))
    }

    action paralyze_player(attacker_index: int, receiver_index: int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.player_class == Monster,
            attacker.hp > 0,
        }, 
        val receiver: Player = new_players_list[receiver_index] all {
            receiver.hp > 0,
            receiver.paralyzed == false,
        }, 
        players_list' = new_players_list.replaceAt(receiver_index, change_paralyze(new_players_list[receiver_index]))
    }

    action blind_monster(attacker_index: int, receiver_index: int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.hp > 0,
            attacker.player_class == Hunter,
        }, 
        val receiver: Player = new_players_list[receiver_index] all {
            receiver.hp > 0,
            receiver.blinded == false,
        }, 
        players_list' = new_players_list.replaceAt(receiver_index, change_blind(new_players_list[receiver_index]))
    }

    action skip_turn(attacker_index : int, new_players_list: List[Player]): bool = all {
        val attacker: Player = new_players_list[attacker_index] all {
            attacker.hp <= 0,
            attacker.player_class != Monster and attacker.paralyzed,
        },
        players_list' = new_players_list 
    }

    // action illusion_time_out(player: Player): bool = all { // was def, error said to say it is an action
    //     player.illusion_on == true,
    //     sheep_counter > 0,
    //     sheep_counter' = sheep_counter - 1,
    // }

    
    // gets rid of all modifiers that last only one turn 
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

        val player_1 = { player_class: Mage, hp: 20, initiative: player_1_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false, playable: true}
        val player_2 = { player_class: Hunter, hp: 20, initiative: player_2_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false, playable: true}
        val player_3 = { player_class: Druid, hp: 20, initiative: player_3_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false, playable: true}
        val player_4 = { player_class: Monster, hp: 100, initiative: player_4_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false, playable: false}
        
        // lista global com a ordem que os caras jogam 
        // ordered_players.append(player_1)

        players_list' = sortList(List(player_1, player_2, player_3, player_4), ordering), 
        turn_number' = 0,
        current_player_index' = 0,
        number_of_players' = 4
    }

    action step = all {
        val new_players_list = players_list.foldl([], (l, p) => if (p == players_list[current_player_index]) l.append(lose_modifiers(players_list[current_player_index])) else l.append(p))
        val monsters_index_list = 0.to(number_of_players).filter(index => players_list[index].player_class == Monster)
        val enemies_index = 0.to(number_of_players).filter(index => can_attack(current_player_index, index))
        val allies_index = 0.to(number_of_players).filter(index => not(can_attack(current_player_index, index)))

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
        current_player_index' = if (current_player_index == number_of_players) 0 else current_player_index + 1
    }

}