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
    val sheep_counter: int = 0
    val turn_number: int = 0 
    val current_player_index: int = 0
    val ordered_players: List[Player] = []

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

    pure def change_transform(receiver: Player): Player = {
        { ...receiver, transformed: true, backup_hp: receiver.hp, hp: 60}
    }

    pure def change_detransform(receiver: Player): Player = {
        { ...receiver, transformed: false, hp: receiver.backup_hp, backup_hp: 0}
    }

    pure def change_illusion(caster: Player): Player = {
        { ...caster, illusion_on: not(caster.illusion_on)} 
    } 

    pure def ordering(p1: Player, p2: Player): bool = {
        p1.initiative < p2.initiative 
    }

    pure def can_attack(attacker: Player, receiver: Player): bool = all {
        attacker.hp > 0, 
        attacker.paralyzed == false, 
        attacker.blinded == false, 
        receiver.hp > 0, 
        if (attacker.player_class == Monster) {
            receiver.player_class != Monster
        }
        else {
            receiver.player_class == Monster
        }
    }

    action deal_damage(attacker: Player, receiver: Player, players_list: List[Player]): bool = {
        val damage = match attacker.player_class {
            | Monster => if (turn_number == 0) 10 else if (attacker.blinded) 0 else 20
            | _ => 10
        }
        all {
            ordered_players' = players_list.foldl([], (acc, p) => if (p == receiver) acc.append(change_hp(p, -damage)) else acc.append(p)),
        }
    }

    action attack_player(attacker: Player, receiver: Player, players_list: List[Player]): bool = all {
        sheep_counter == 0, 
        val bears: List[Player] = players_list.foldl([], (l, p) => if (p.transformed) l.append(p) else l)
        bears.length() != 0, // verificando se nao tem bears (da certo?)
        deal_damage(attacker, receiver, players_list),
    }
    
    action attack_sheep(attacker: Player, players_list: List[Player]): bool = all {
        attacker.player_class == Monster, 
        sheep_counter > 0,  
        sheep_counter' = sheep_counter - 1, 
        ordered_players' = players_list.foldl([], (acc, p) => if (p.player_class == Mage and p.illusion_on) acc.append(change_illusion(p)) else acc.append(p)),
    }

    action attack_bear(attacker: Player, players_list: List[Player]): bool = all {
        attacker.player_class == Monster, 
        val bears: List[Player] = players_list.foldl([], (l, p) => if (p.transformed) l.append(p) else l)
        bears != [], 
        // escolher algum bear => se hp ficar <= 0 detransform 
    }

    action bear_transform(player: Player, players_list: List[Player]): bool = all {
        player.player_class == Druid, 
        player.transformed == false, 
        player.hp > 0, 
        ordered_players' = players_list.foldl([], (acc, p) => if (p == player) acc.append(change_transform(p)) else acc.append(p)),
    }

    action create_illusion(player: Player, players_list: List[Player]): bool = all {
        player.player_class == Mage, 
        player.illusion_on == false, 
        player.hp > 0, 
        sheep_counter' = sheep_counter + 1,
        ordered_players' = players_list.foldl([], (acc, p) => if (p == player) acc.append(change_illusion(p)) else acc.append(p)),
    }

    action remove_paralysis(player: Player, allies: Set[Player], players_list: List[Player]): bool = all {
        val paralyzed = allies.filter(p => p.paralyzed and p.hp > 0)
        nondet receiver = oneOf(paralyzed)

        // nao tenho certeza se isso da certo
        ordered_players' = players_list.foldl([], (acc, p) => if (p == receiver) acc.append(change_paralyze(p)) else acc.append(p)),
        player.player_class != Monster, 
    }

    action paralyze_player(attacker: Player, receiver: Player, players_list: List[Player]): bool = all {
        attacker.player_class == Monster,
        attacker.hp > 0,
        receiver.hp > 0,
        receiver.paralyzed == false,
        ordered_players' = players_list.foldl([], (acc, p) => if (p == receiver) acc.append(change_paralyze(p)) else acc.append(p)),
    }

    action blind_monster(attacker: Player, receiver: Player, players_list: List[Player]): bool = all {
        attacker.player_class == Hunter,
        attacker.hp > 0,
        receiver.hp > 0,
        receiver.blinded == false,
        ordered_players' = players_list.foldl([], (acc, p) => if (p == receiver) acc.append(change_blind(p)) else acc.append(p)),
    }

    action skip_turn(player: Player): bool = all {
        player.hp <= 0,
        player.player_class != Monster and player.paralyzed,
    }

    def illusion_time_out(): bool = all {
        sheep_counter > 0,
        sheep_counter' = sheep_counter - 1,
    }

    def lose_modifiers(player: Player): List[Player] = {
        match player.player_class {
            | Mage => ordered_players.foldl([], (acc, p) => acc.append(if (p.illusion_on and illusion_time_out) change_illusion(p) else p)) // nao sei se funciona
            | Druid => ordered_players.foldl([], (acc, p) => acc.append(if (p.transformed) change_detransform(p) else p))
            | Monster => ordered_players.foldl([], (acc, p) => acc.append(if (p.blinded) change_blind(p) else p))
            | _ => ordered_players
        }
    }

    // action init = {
    //     pure val d20 = 1.to(20) 
    //     nondet player_1_initiative = d20.oneOf()
    //     nondet player_2_initiative = d20.oneOf()
    //     nondet player_3_initiative = d20.oneOf()
    //     nondet player_4_initiative = d20.oneOf()

    //     val player_1 = { player_class: Mage, hp: 20, initiative: player_1_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
    //     val player_2 = { player_class: Hunter, hp: 20, initiative: player_2_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
    //     val player_3 = { player_class: Druid, hp: 20, initiative: player_3_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
    //     val player_4 = { player_class: Monster, hp: 100, initiative: player_4_initiative, paralyzed: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        
    //     // lista global com a ordem que os caras jogam 
    //     // ordered_players.append(player_1)
    //     // sortList(ordered_players, ordering)

    //     all {
    //         ordered_players' = List(player_1, player_2, player_3, player_4),
    //         turn_number' = 0,
    //         current_player_index' = 0,
    //         sheep_counter' = 0,
    //     }
    // }

    action step = all {
        val current_player = ordered_players[current_player_index]
        val players_list = lose_modifiers(current_player)
        val players_set = players_list.indices().map(i => players_list[i])
        val enemies = players_set.filter(possible_enemy => can_attack(current_player, possible_enemy))
        val allies = players_set.filter(possible_ally => not(can_attack(current_player, possible_ally)))
        nondet current_enemy = oneOf(enemies)
        any {
            attack_sheep(current_player, players_list),
            attack_bear(current_player, players_list),
            attack_player(current_player, current_enemy, players_list), // atacar monstro ou personagem (nao transformado)
            bear_transform(current_player, players_list),
            create_illusion(current_player, players_list),
            remove_paralysis(current_player, allies, players_list),
            paralyze_player(current_player, current_enemy, players_list),
            blind_monster(current_player, current_enemy, players_list),
            skip_turn(current_player),
        },

        turn_number' = turn_number + 1
    }

}