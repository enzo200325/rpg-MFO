module rpg {
    import basicSpells.* from "./spells/basicSpells"

    type Class = Mage | Druid | Hunter | Monster 
    type Player = {
        player_class: Class, 
        playable: bool, 
        hp: int, 
        initiative: int, 
        paralized: bool, 
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
    
    pure def change_paralize(receiver: Player): Player = {
        { ...receiver, paralized: true}
    }

    pure def change_blind(receiver: Player): Player = {
        { ...receiver, blinded: true}
    }

    // transform (bomba)

    pure def change_illusion(caster: Player): Player = {
        { ...caster, illusion_on: true }
    } 

    pure def ordering(p1: Player, p2: Player): bool = {
        p1.initiative < p2.initiative 
    }
    
    action init = {
        pure val d20 = 1.to(20) 
        nondet player_1_initiative = d20.oneOf()
        nondet player_2_initiative = d20.oneOf()
        nondet player_3_initiative = d20.oneOf()
        nondet player_4_initiative = d20.oneOf()

        val player_1 = { player_class: Mage, hp: 20, initiative: player_1_initiative, paralized: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        val player_2 = { player_class: Hunter, hp: 20, initiative: player_2_initiative, paralized: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        val player_3 = { player_class: Druid, hp: 20, initiative: player_3_initiative, paralized: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        val player_4 = { player_class: Monster, hp: 100, initiative: player_4_initiative, paralized: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        
        // lista global com a ordem que os caras jogam 
        ordered_players.append(player_1)
        sortList(ordered_players, ordering)

        all {
            ordered_players' = ordered_players,
            turn_number' = 0,
            current_player_index' = 0,
            sheep_counter' = 0,
        }
    }

    pure def can_attack(attacker: Player, receiver: Player): bool = all {
        attacker.hp > 0, 
        attacker.paralized == false, 
        attacker.blinded == false, 
        receiver.hp > 0, 
        if (attacker.player_class == Monster) {
            receiver.player_class != Monster
        }
        else {
            receiver.player_class == Monster
        }
    }

    action deal_damage(attacker: Player, receiver: Player): bool = all {
        val damage = match attacker.player_class {
            | Monster => if (turn_number == 0) 10 else 20
            | _ => 10
        }
        change_hp(receiver, -damage),
    }

    action attack_player(attacker: Player, receiver: Player): bool = all {
        sheep_counter == 0, 
        val bears: List[Player] = ordered_players.foldl([], (l, p) => if (p.transformed) l.append(p) else l)
        bears.length() != 0, // verificando se nao tem bears (da certo?)
        deal_damage(attacker, receiver),
    }
    
    action attack_sheep(attacker: Player): bool = all {
        attacker.player_class == Monster, 
        sheep_counter > 0,  
        sheep_counter' = sheep_counter - 1, 
    }

    action attack_bear(attacker: Player): bool = all {
        attacker.player_class == Monster, 
        val bears: List[Player] = ordered_players.foldl([], (l, p) => if (p.transformed) l.append(p) else l)
        bears != [], 
        // escolher algum bear => se hp ficar <= 0 detransform 
    }

    action step = all {
        val current_player = ordered_players[current_player_index]
        val enemies = ordered_players.filter(possible_enemy => can_attack(current_player, possible_enemy))
        nondet current_enemy = oneOf(enemies)
        any {
            attack_sheep(current_player),
            attack_bear(current_player),
            attack_player(current_player, current_enemy), // atacar monstro ou personagem (nao transformado)
        },

        turn_number' = turn_number + 1
    }

}