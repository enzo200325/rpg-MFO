module rpg {
    import basicSpells.* from "./spells/basicSpells"

    type Class = Mage | Druid | Hunter | Monster
    type Player = {
        player_class: Class, 
        hp: int, 
        initiative: int, 
        paralized: bool, 
        blinded: bool, 
        backup_hp: int, 
        transformed: bool, 
        illusion_on: bool 

    }
    val sheep_counter: int = 0
    val first_turn: bool = false 
    val current_player_index: int = 0

    type Modifier = Ilusion | Blindness | Transform

    pure def change_hp(receiver: Player, change: int): Player = {
        { ...receiver, hp: receiver.hp + change}
    }
    pure def paralize(receiver: Player): Player = {
        { ...receiver, paralized: true}
    }
    pure def blind(receiver: Player): Player = {
        { ...receiver, blind: true}
    }
    // transform (bomba)
    pure def illusion(caster: Player): Player = {
        { ...caster, illusion_on: true }
    }

    val ordered_players : List[Player] 

    pure def ordering(p1: Player, p2: Player): bool = {
        p1.initiative < p2.initiative 
    }
    action init = all {
        val player_1 = { player_class: Mage, hp: 20, initiative: oneOf(1.to(20)), paralized: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        val player_2 = { player_class: Hunter, hp: 20, initiative: oneOf(1.to(20)), paralized: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        val player_3 = { player_class: Druid, hp: 20, initiative: oneOf(1.to(20)), paralized: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}
        val player_4 = { player_class: Monster, hp: 100, initiative: oneOf(1.to(20)), paralized: false, blinded: false, backup_hp: 0, transformed: false, illusion_on: false}

        // lista global com a ordem que os caras jogam 
        sortList(ordered_players, ordering), 
        first_turn' = true, 
        sheep_counter' = 0, 
        current_player_index' = 0 
    }

    action step = all {
        any {

        }
    }

}