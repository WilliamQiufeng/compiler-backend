mod block;
mod ir;
mod live_lattice;
mod reach_lattice;
mod semilattice;
fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {

    use crate::block::Direction::Forward;
    use crate::block::{BlockLattice, BlockUpdate, DataFlowGraph};
    use crate::ir::JumpType::{Bool, E};
    use crate::ir::QuadType::{Add, Assign, Sub};
    use crate::ir::Value::{Const, Variable};
    use crate::ir::IR::{Jump, Quad};
    use crate::ir::{
        AddressMarker, CodeBlock, CodeBlockGraphWeight, IRInformation, QuadType, Value,
    };
    use crate::reach_lattice::ReachLattice;
    use crate::semilattice::SemiLattice;

    mod u32_lattice;
    #[test]
    fn it_works() {
        let x = u32_lattice::U32SemiLattice::from(0b101101u32);
        let y = u32_lattice::U32SemiLattice::from(0b110011u32);
        let res = x.meet(&y);
        assert_eq!(res, 0b111111u32.into());
        assert_eq!(x.meet(&0.into()), x);
        assert_eq!(x.meet(&u32::MAX.into()), u32::MAX.into());
        assert!(x < 0.into())
    }
    #[test]
    fn graph() {
        let mut graph = DataFlowGraph::<u32_lattice::U32Block>::new(());
        let b1 = graph.graph.add_node(u32_lattice::U32Block::new(
            1.into(),
            0b10010.into(),
            0b01101.into(),
        ));
        let b2 = graph.graph.add_node(u32_lattice::U32Block::new(
            2.into(),
            0b01110.into(),
            0b01110.into(),
        ));
        graph.graph.add_edge(graph.entry, b1, ());
        graph.graph.add_edge(b1, b2, ());
        graph.converge(crate::block::Direction::Forward);
        graph.graph.node_indices().for_each(|i| {
            let node = graph.graph.node_weight(i).unwrap();
            println!("{}, {}", node.get_in().0, node.get_out().0);
        });
    }
    #[test]
    fn block_partition() {
        let i = 0;
        let j = 1;
        let k = 2;
        let t1 = 3;
        let irs = vec![
            Quad(Assign, i, Const(1), Value::None, IRInformation::default()),
            Quad(Assign, j, Const(1), Value::None, IRInformation::default()),
            Quad(Assign, k, Const(2), Value::None, IRInformation::default()),
            Jump(
                E,
                AddressMarker::new(5),
                Variable(j),
                Variable(k),
                IRInformation::default(),
            ),
            Quad(
                QuadType::E,
                t1,
                Variable(j),
                Variable(k),
                IRInformation::default(),
            ),
            Quad(
                Assign,
                k,
                Variable(t1),
                Value::None,
                IRInformation::default(),
            ),
            Quad(
                Assign,
                k,
                Variable(t1),
                Value::None,
                IRInformation::default(),
            ),
            Jump(
                Bool,
                AddressMarker::new(1),
                Variable(t1),
                Value::None,
                IRInformation::default(),
            ),
            Quad(
                Assign,
                k,
                Variable(t1),
                Value::None,
                IRInformation::default(),
            ),
        ];
        let mut partitioned = DataFlowGraph::from(irs);
        <DataFlowGraph<CodeBlock, CodeBlockGraphWeight> as BlockUpdate<ReachLattice>>::converge(
            &mut partitioned,
            Forward,
        );
        println!("{:}", partitioned)
    }

    #[test]
    fn block_reach() {
        let i = 0;
        let j = 1;
        let a = 2;
        let m = 3;
        let n = 4;
        let u1 = 5;
        let u2 = 6;
        let u3 = 7;
        let irs = vec![
            Quad(Sub, i, Variable(m), Const(1), IRInformation::default()),
            Quad(
                Assign,
                j,
                Variable(n),
                Value::None,
                IRInformation::default(),
            ),
            Quad(
                Assign,
                a,
                Variable(u1),
                Value::None,
                IRInformation::default(),
            ),
            Quad(Add, i, Variable(i), Const(1), IRInformation::default()),
            Quad(Sub, j, Variable(j), Const(1), IRInformation::default()),
            Jump(
                Bool,
                AddressMarker::new(7),
                Value::None,
                Value::None,
                IRInformation::default(),
            ),
            Quad(
                Assign,
                a,
                Variable(u2),
                Value::None,
                IRInformation::default(),
            ),
            Quad(
                Assign,
                i,
                Variable(u3),
                Value::None,
                IRInformation::default(),
            ),
            Jump(
                Bool,
                AddressMarker::new(3),
                Value::None,
                Value::None,
                IRInformation::default(),
            ),
        ];
        let mut partitioned = DataFlowGraph::from(irs);
        <DataFlowGraph<CodeBlock, CodeBlockGraphWeight> as BlockUpdate<ReachLattice>>::converge(
            &mut partitioned,
            Forward,
        );
        println!("{:}", partitioned)
    }
}
